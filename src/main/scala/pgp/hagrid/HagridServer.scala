package pgp.hagrid

import java.io.IOException
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import io.circe.parser.decode
import pgp.hagrid.JsonProviders._
import pgp.{Identity => PgpIdentity, _}
import sttp.client
import sttp.client._
import sttp.client.circe._
import sttp.model.Uri

import scala.io.Source
import scala.util.matching.Regex


case class UploadBody(keytext: String)

case class UploadResponse(token: String,
                          key_fpr: String,
                          status: Map[String, String])

case class VerifyRequest(token: String, addresses: List[String])

case class HagridEnvelope(
                           forward_path: Array[String],
                           reverse_path: String
                         )

case class HagridMail(
                       envelope: HagridEnvelope,
                       message_id: String,
                       message: Array[Byte]
                     )

object HagridServer {

  private val mailWatcher = FileSystems.getDefault.newWatchService


  val baseURL = "http://0.0.0.0:8080"
  val mailPath: Path = Paths.get("/home/lukas/fakemail/mails/")
  val hagridPath: Path = Paths.get("/home/lukas/hagrid/hagrid/state")

  val REVOKE_PATTERN: Regex = "(?s).*To: <(\\S+)>.*OpenPGP key: (\\S+).*http://localhost:8080/manage/(\\S+).*<!doctype html>.*".r
  val VERIFY_PATTERN: Regex = "(?s).*To: <(\\S+)>.*OpenPGP key: (\\S+).*http://localhost:8080/verify/(\\S+).*<!doctype html>.*".r


  def remove(root: Path): Unit = {
    if (root.toFile.exists) {
      Files.walkFileTree(root, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    }
  }


  def parseMail(mail: String): Seq[Body] =
    decode[HagridMail](mail)
      .map(mail => new String(mail.message))
      .map {
        case REVOKE_PATTERN(identity, fingerprint, token) => Body(FingerprintImpl(fingerprint), Token(token), PgpIdentity(identity))
        case VERIFY_PATTERN(identity, fingerprint, token) => Body(FingerprintImpl(fingerprint), Token(token), PgpIdentity(identity))
      }
      .toSeq


  def hag(location: String): Uri = {
    val url = s"$baseURL$location"
    uri"$url"
  }

  implicit val backend: SttpBackend[client.Identity, Nothing, NothingT] =
    HttpURLConnectionBackend()

}


/**
 * For the sake of simplicity this remote server spec is synchronous and thus blocking.
 */
class HagridServer extends Spec1 {

  import HagridServer._

  import scala.collection.JavaConverters._


  //remove(hagridPath)

  mailPath.register(
    mailWatcher,
    StandardWatchEventKinds.ENTRY_CREATE
  )




  /**
   * Read the file containing the mail body, then turn it into a Body and finally delete the file.
   * May throw an exception if the file cannot be read or deleted.
   * Here be dragons.
   */
  private def consumeMail(expectedSize: Int): Seq[Body] = {
    println(s"Waiting for modifications in path:$mailPath ")
    // This blocks until the file is modified
    println(s"Expected size: $expectedSize")
    val watchKey = mailWatcher.take
    val events = watchKey.pollEvents.asScala
    println(events.size)
    val updatePaths = events
      .take(expectedSize)
      .map(_.asInstanceOf[WatchEvent[Path]])
      .map(_.context())

    updatePaths.foreach(println(_))

    val mails = updatePaths.flatMap { currentPath =>
      val resolved = mailPath.resolve(currentPath).toFile

      println(resolved)

      val source = Source.fromFile(resolved)
      val bodies: Seq[Body] = parseMail(source.mkString)

      source.close
      resolved.delete

      val isValid = watchKey.reset


      bodies
    }

    println(s"Returning ${mails.size} mails")

    mails

  }


  override def byEmail(identity: PgpIdentity): Option[Key] = for {
    body <- basicRequest
      .get(hag(s"/vks/v1/by-email/${identity.email}"))
      .send()
      .body
      .toOption
  } yield KeyGenerator.fromArmored(body)

  override def byFingerprint(fingerprint: Fingerprint): Option[Key] = for {
    body <- basicRequest
      .get(hag(s"/vks/v1/by-fingerprint/${fingerprint.value}"))
      .send()
      .body
      .toOption
  } yield KeyGenerator.fromArmored(body)

  override def byKeyId(keyId: KeyId): Iterable[Key] = for {
    body <- basicRequest
      .get(hag(s"/vks/v1/by-fingerprint/${keyId.value}"))
      .send()
      .body
      .toSeq
  } yield KeyGenerator.fromArmored(body)

  override def upload(key: Key): Token = {
    val response = basicRequest
      .post(hag("/vks/v1/upload"))
      .body(UploadBody(key.armored))
      .response(asJson[UploadResponse])
      .send()
      .body


    response.left.map(e => println(e.body))

    Token(response
      .toOption
      .get
      .token)

  }

  def checkResponse(mails: Set[String], response: Option[String]): Boolean = response.flatMap { r =>
    decode[UploadResponse](r)
      .left.map(println(_))
      .toOption
      .map(_.status)
      .map { states =>
        mails.exists { m =>
          !states.get(m).contains("published")
        }
      }
  }.getOrElse(false)

  override def requestVerify(from: Token,
                             emails: Set[PgpIdentity]): Seq[Body] = {

    if (emails.isEmpty) return Seq.empty

    val response = basicRequest
      .post(hag("/vks/v1/request-verify"))
      .body(VerifyRequest(from.uuid, emails.map(_.email).toList))
      .send()
      .body
      .toOption

    val shouldReadMail = checkResponse(emails.map(_.email), response)


    (response, shouldReadMail) match {
      case (Some(x), true) => consumeMail(emails.size)
      case _ => Seq.empty
    }
  }

  override def verify(token: Token): Unit =
    basicRequest
      .post(hag(s"/verify/${token.uuid}"))
      .send()

  override def requestManage(identity: PgpIdentity): Option[EMail] = for {
    _ <- basicRequest
      .post(hag("/manage"))
      .body(Map("search_term" -> identity.email))
      .send()
      .body
      .toOption

    mail <- consumeMail(1)
      .headOption
      .map { case Body(fingerprint, token, identity) => EMail("", fingerprint, token) }

  } yield mail

  /**
   * This seems to be completely undocumented?
   * I did find a possible endpoint in the hagrid source: manage/unpublish.
   */
  override def revoke(token: Token, emails: Set[PgpIdentity]): Unit =
    for (identity <- emails) {
      basicRequest
        .post(hag("/manage/unpublish"))
        .body(
          Map("token" -> token.uuid.toString, "address" -> identity.email) // WIP
        )
        .send()
    }

}

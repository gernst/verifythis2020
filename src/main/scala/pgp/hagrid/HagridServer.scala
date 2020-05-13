package pgp.hagrid

import java.nio.file.{FileSystems, Path, StandardWatchEventKinds}

import pgp.{Identity => PgpIdentity, _}
import sttp.client
import sttp.client._
import sttp.client.circe._
import sttp.model.Uri

import scala.io.Source

case class UploadResponse(token: Token,
                          key_fpr: Fingerprint,
                          status: Map[String, String])

case class VerifyRequest(token: Token, addresses: List[PgpIdentity])

object HagridServer {
  val baseURL = "127.0.0.1:8000"
  val mailPath: Path = ???

  /**
   *
   * FOR VERIFY:
   * Parsing this will not be fun:
   *
   * To: lukasrieger07@gmail.com <-- Identity to be verified / revoked
   * Hi,
   *
   * Dies ist eine automatische Nachricht von localhost.
   * Falls dies unerwartet ist, bitte die Nachricht ignorieren.
   *
   * OpenPGP Schlüssel: 23B2E0C54487F50AC59134C3A1EC9765D7B25C5A <---- PARSE THIS: Fingerprint
   *
   * Damit der Schlüssel über die Email-Adresse "lukasrieger07@gmail.com" gefunden werden kann,
   * klicke den folgenden Link:
   *
   * http://localhost:8080/verify/vrTohvV8q552KMvARBE7foqkvGtUrfDl3iiyX9yqeOX <---- PARSE THIS: Token
   *
   * Weiter Informationen findest du unter http://localhost:8080/about
   *
   * --
   *
   * FOR REVOKE:
   *
   * To: lukasrieger07@gmail.com
   * Hi,
   *
   * Dies ist eine automatische Nachricht von localhost.
   * Falls dies unerwartet ist, bitte die Nachricht ignorieren.
   *
   * OpenPGP Schlüssel: 23B2E0C54487F50AC59134C3A1EC9765D7B25C5A
   *
   * Du kannst die Identitäten dieses Schlüssels unter folgendem Link verwalten:
   *
   * http://localhost:8080/manage/AQKKXsNS9pSBKjyFZdR7FmLZCht_N8o5EWHeehuYyBtDgpBxx2J-bUFe0Q4R9_ZFKKKgnOImmKhbhHwGkhq8LzqfqAnINTEMCAarga5k6m8gMEVIlJtFWmZ-BuSitHjmnLUcsBycd9yH
   *
   * Weiter Informationen findest du unter http://localhost:8080/about
   *
   * --
   */
  def parseMail(mail: String): Seq[Body] = ???

  def hag(location: String): Uri = {
    val url = s"$baseURL$location"
    uri"url"
  }

  implicit val backend: SttpBackend[client.Identity, Nothing, NothingT] =
    HttpURLConnectionBackend()

}


/**
 * For the sake of simplicity this remote server spec is synchronous and thus blocking.
 */
class HagridServer extends Spec1 {

  import HagridServer._
  import JsonProviders._

  import scala.collection.JavaConverters._

  private val mailWatcher = FileSystems.getDefault.newWatchService


  mailPath.register(
    mailWatcher,
    StandardWatchEventKinds.ENTRY_CREATE,
    StandardWatchEventKinds.ENTRY_MODIFY,
    StandardWatchEventKinds.ENTRY_DELETE
  )


  override def byEmail(identity: PgpIdentity): Option[Key] =
    basicRequest
      .get(hag(s"/vks/v1/by-email/${identity.email}"))
      .response(asJson[Option[Key]])
      .send()
      .body
      .toOption
      .flatten

  override def byFingerprint(fingerprint: Fingerprint): Option[Key] =
    basicRequest
      .get(hag(s"/vks/v1/by-fingerprint/${fingerprint.value}"))
      .response(asJson[Option[Key]])
      .send()
      .body
      .toOption
      .flatten

  override def byKeyId(keyId: KeyId): Iterable[Key] =
    basicRequest
      .get(hag(s"/vks/v1/by-fingerprint/${keyId.value}"))
      .response(asJson[Iterable[Key]])
      .send()
      .body
      .toOption
      .get

  override def upload(key: Key): Token =
    basicRequest
      .post(hag("/vks/v1/upload"))
      .body(key)
      .response(asJson[UploadResponse])
      .send()
      .body
      .toOption
      .get
      .token

  override def requestVerify(from: Token,
                             emails: Set[PgpIdentity]): Seq[Body] = {

    basicRequest
      .post(hag("/vks/v1/request-verify"))
      .body(VerifyRequest(from, emails.toList))
      .send()

    // This blocks until the file is modified
    val events = mailWatcher.take.pollEvents.asScala
    val source = Source.fromFile(mailPath.toFile)
    val bodies: Seq[Body] = parseMail(source.mkString)

    source.close

    bodies
  }

  override def verify(token: Token): Unit =
    basicRequest
      .post(hag("/verify"))
      .body(token)
      .send()

  override def requestManage(identity: PgpIdentity): Option[EMail] = {
    basicRequest
      .post(hag("/manage"))
      .body(Map("search_term" -> identity.email))
      .send()
      .body

    // This blocks until the file is modified
    val events = mailWatcher.take.pollEvents.asScala
    val source = Source.fromFile(mailPath.toFile)
    val bodies: Seq[Body] = parseMail(source.mkString)

    source.close

    bodies
      .headOption
      .map { case Body(fingerprint, token, identity) => EMail("", fingerprint, token) }

  }

  /**
   * This seems to be completely undocumented?
   * I did find a possible endpoint in the hagrid source: manage/unpublish.
   */
  override def revoke(token: Token, emails: Set[PgpIdentity]): Unit =
    for (identity <- emails)
      basicRequest
        .post(hag("manage/unpublish"))
        .body(
          Map("token" -> token.uuid.toString, "address" -> identity.email) // WIP
        )
        .send()

}

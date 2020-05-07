package pgp.hagrid

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

  /**
   * Parsing this will not be fun.
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

    val source = Source.fromFile("path/to/named/pipe/maybe?")
    parseMail(source.mkString) // might not be the right thing to do. Should test this!
  }

  override def verify(token: Token): Unit =
    basicRequest
      .post(hag("/verify"))
      .body(token)
      .send()

  override def requestManage(identity: PgpIdentity): Option[EMail] =
    basicRequest
      .post(hag("/manage"))
      .body(Map("search_term" -> identity.email))
      .response(asJson[Option[EMail]])
      .send()
      .body
      .toOption
      .flatten

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

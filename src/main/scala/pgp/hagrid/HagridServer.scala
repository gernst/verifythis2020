package pgp.hagrid


import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import pgp.{Identity => PgpIdentity, _}
import sttp.client
import sttp.client._
import sttp.client.circe._
import sttp.model.Uri


case class UploadResponse(token: Token, key_fpr: Fingerprint, status: Map[String, String])
case class VerifyRequest(token: Token, addresses: List[PgpIdentity])


object PgpDerivations {

  implicit val keyDecoder: Decoder[Key] = deriveDecoder[Key]
  implicit val keyIdDecoder: Decoder[KeyId] = deriveDecoder[KeyId]
  implicit val fingerprintDecoder: Decoder[Fingerprint] = deriveDecoder[Fingerprint]
  implicit val identityDecoder: Decoder[PgpIdentity] = deriveDecoder[PgpIdentity]

  implicit val keyEncoder: Encoder[Key] = deriveEncoder[Key]
  implicit val keyIdEncoder: Encoder[KeyId] = deriveEncoder[KeyId]
  implicit val fingerprintEncoder: Encoder[Fingerprint] = deriveEncoder[Fingerprint]
  implicit val identityEncoder: Encoder[PgpIdentity] = deriveEncoder[PgpIdentity]


  implicit val tokenEncoder: Encoder[Token] = deriveEncoder[Token]
  implicit val tokenDecoder: Decoder[Token] = deriveDecoder[Token]

  implicit val uploadDecoder: Decoder[UploadResponse] = deriveDecoder[UploadResponse]
  implicit val keyBodyEncoder: BodySerializer[Key] = ???

}


object HagridServer {
  val baseURL = "127.0.0.1:8000"


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
  import PgpDerivations._

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


  override def requestVerify(from: Token, emails: Set[PgpIdentity]): Seq[Body] = ???

  //    basicRequest
  //      .post(hag("/vks/v1/request-verify"))
  //      .body(VerifyRequest(from, emails.toList))


  override def verify(token: Token): Unit = ???

  override def requestManage(identity: PgpIdentity): Option[EMail] = ???

  //    basicRequest
  //      .post(hag("manage"))
  //      .body(Map("search_term" -> identity.email))
  //      .send()
  //      .body
  //      .toOption
  //      .get

  override def revoke(token: Token, emails: Set[PgpIdentity]): Unit = ???
}



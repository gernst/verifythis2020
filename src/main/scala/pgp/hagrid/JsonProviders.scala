package pgp.hagrid

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import pgp._
import sttp.client.BodySerializer

object JsonProviders {
  implicit val keyDecoder: Decoder[Key] = deriveDecoder[Key]
  implicit val keyIdDecoder: Decoder[KeyId] = deriveDecoder[KeyId]
  implicit val fingerprintDecoder: Decoder[Fingerprint] =
    deriveDecoder[Fingerprint]
  implicit val identityDecoder: Decoder[Identity] =
    deriveDecoder[Identity]

  implicit val keyEncoder: Encoder[Key] = deriveEncoder[Key]
  implicit val keyIdEncoder: Encoder[KeyId] = deriveEncoder[KeyId]
  implicit val fingerprintEncoder: Encoder[Fingerprint] =
    deriveEncoder[Fingerprint]
  implicit val identityEncoder: Encoder[Identity] =
    deriveEncoder[Identity]

  implicit val tokenEncoder: Encoder[Token] = deriveEncoder[Token]
  implicit val tokenDecoder: Decoder[Token] = deriveDecoder[Token]

  implicit val uploadDecoder: Decoder[UploadResponse] =
    deriveDecoder[UploadResponse]
  implicit val keyBodyEncoder: BodySerializer[Key] = ???

  implicit val verifyEncoder: Encoder[VerifyRequest] =
    deriveEncoder[VerifyRequest]

  implicit val mailDecoder: Decoder[EMail] = deriveDecoder[EMail]

}

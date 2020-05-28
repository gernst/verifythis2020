package pgp.hagrid

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import pgp._

object JsonProviders {

  implicit val hagridEnvelopeDecoder: Decoder[HagridEnvelope] = deriveDecoder[HagridEnvelope]
  implicit val hagridMailDecoder: Decoder[HagridMail] = deriveDecoder[HagridMail]
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
  implicit val uploadBodyEncoder: Encoder[UploadBody] = deriveEncoder[UploadBody]

  implicit val verifyEncoder: Encoder[VerifyRequest] =
    deriveEncoder[VerifyRequest]

  implicit val mailDecoder: Decoder[EMail] = deriveDecoder[EMail]

}

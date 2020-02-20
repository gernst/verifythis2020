package pgp.types

import java.util.UUID


sealed trait KeyId

sealed trait Fingerprint


/**
 * Having the key as a trait might actually not be a good idea.
 * It forces the server to relay the downsizing of currently validated identities to the key itself.
 * This is mainly due to the fact that the server doesn't know the actual implementation of this trait at runtime
 * and thus the server cannot construct a new key from the existing one.
 *
 * While this is not a big problem at the moment, it is reasonable to assume that any "downsizing" will usually
 * only involve filtering the set of identities that are present in the key down to the ones present in the set of
 * validated identities currently managed by the server.
 */
sealed trait Key {
  def keyId: KeyId

  def fingerprint: Fingerprint

  def identities: Set[Identity]

  def restrictedTo(ids: Set[Identity]): Key

}

final case class PGPKey(keyId: KeyId,
                  fingerprint: Fingerprint,
                  identities: Set[Identity]
                 ) extends Key {


  override def restrictedTo(ids: Set[Identity]): Key = {
    assert(ids subsetOf identities)
    PGPKey(keyId,fingerprint,ids)
  }
}

object Key {
  def random(ids: Set[Identity]): Key =
    PGPKey(
      keyId = KeyId.random,
      fingerprint = Fingerprint.random,
      identities = ids
    )
}

final case class EMail(message: String, fingerprint: Fingerprint, token: Token)

final case class FingerprintImpl(fingerprint: String) extends Fingerprint

object Fingerprint {
  def random: Fingerprint = FingerprintImpl(UUID.randomUUID().toString)
}

final case class KeyIdImpl(id: String) extends KeyId

object KeyId {
  def random: KeyId = KeyIdImpl(UUID.randomUUID().toString)
}

case class Identity(email: String)

object Identity {
  def mails(): Iterable[String] = Iterable(
    "ilyaz@comcast.net",
    "erynf@comcast.net",
    "phish@verizon.net",
    "empathy@yahoo.ca",
    "peoplesr@optonline.net",
    "crowl@verizon.net",
    "ranasta@live.com",
    "rupak@mac.com",
    "wonderkid@yahoo.com",
    "eminence@hotmail.com",
    "crusader@sbcglobal.net",
    "tezbo@att.net",
    "mailarc@yahoo.com",
    "majordick@me.com",
    "jaffe@aol.com",
    "mschilli@live.com",
    "whimsy@yahoo.com",
    "boser@yahoo.ca",
    "bulletin@optonline.net",
    "jonas@yahoo.ca",
    "gator@hotmail.com",
    "isotopian@outlook.com",
    "formis@aol.com",
    "hutton@outlook.com",
    "fviegas@outlook.com",
    "dkasak@msn.com",
    "sopwith@live.com",
    "horrocks@me.com",
    "tfinniga@comcast.net",
    "gfxguy@sbcglobal.net")
}

/**
 * The PGP reference actually defines the fingerprint as subset of the keyId bytes.
 * This is unnecessary for such an abstract model of a keyserver so both keyId and fingerprint
 * are currently implemented as pseudo random sequences with sufficient length to ensure a "low enough" probability
 * of collisions with other keys.
 *
 * NOTE: For now this simply uses the same UUIDs that are being used for token generation
 */



// Uses type 4 UUIDs with 122 bits of strong randomness.
// Proposed by: https://github.com/wadoon/keyserver-java/
final case class Token(uuid: UUID)

object Token {
  def unique: Token = {
    Token(UUID.randomUUID)
  }
}
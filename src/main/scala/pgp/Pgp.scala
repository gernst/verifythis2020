package pgp

import java.nio.ByteBuffer
import java.util.UUID

import pgp.hagrid.KeyGenerator

case class Body(fingerprint: Fingerprint, token: Token, identity: Identity)

sealed trait Data

case class Mail(from: Actor, to: Identity, body: Body) extends Data

case class Packet(from: Actor, to: Actor, msg: Message) extends Data

sealed trait Message

object Init extends Message

sealed trait ServerMessage extends Message
case class Manage(email: EMail) extends ServerMessage
case class Verification(email: EMail) extends ServerMessage
case class FromFingerprint(key: Option[Key]) extends ServerMessage
case class FromKeyId(key: Iterable[Key]) extends ServerMessage
case class FromEmail(key: Option[Key]) extends ServerMessage
case class Uploaded(token: Token) extends ServerMessage

sealed trait ClientMessage extends Message
case class Upload(key: Key) extends ClientMessage
case class ByFingerprint(fingerprint: Fingerprint) extends ClientMessage
case class ByKeyId(keyId: KeyId) extends ClientMessage
case class ByEmail(identity: Identity) extends ClientMessage
case class RequestVerify(from: Token, identity: Identity) extends ClientMessage
case class Verify(token: Token) extends ClientMessage
case class RequestManage(identity: Identity) extends ClientMessage
case class Revoke(token: Token, identities: Set[Identity]) extends ClientMessage

sealed trait KeyId {
  def value: String
}

sealed trait Fingerprint {
  def value: String
}

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

case class PGPKey(keyId: KeyId,
                  fingerprint: Fingerprint,
                  identities: Set[Identity],
                  armored: String
                 )
  extends Key {

  override def restrictedTo(ids: Set[Identity]): Key = {
    assert(ids subsetOf identities)
    PGPKey(keyId, fingerprint, ids, armored)
  }
}

object Key {
  def random(ids: Set[Identity]): Key =
    PGPKey(
      keyId = KeyId.random,
      fingerprint = Fingerprint.random,
      identities = ids,
      armored = ""
    )

  def pgp(ids: Set[Identity]): Key = {
    val (pgpKey, armored) = KeyGenerator.genPublicKey(ids)

    PGPKey(
      keyId = KeyIdImpl(pgpKey.getKeyID.toString),
      fingerprint = FingerprintImpl(ByteBuffer.wrap(pgpKey.getFingerprint).getLong.toString),
      identities = ids,
      armored = armored
    )
  }
}

case class EMail(message: String, fingerprint: Fingerprint, token: Token)

case class FingerprintImpl(fingerprint: String) extends Fingerprint {
  def value: String = fingerprint
}

object Fingerprint {
  def random: Fingerprint = FingerprintImpl(UUID.randomUUID().toString)
}

case class KeyIdImpl(id: String) extends KeyId {
  def value: String = id
}

object KeyId {
  def random: KeyId = KeyIdImpl(UUID.randomUUID().toString)
}

case class Identity(email: String)

object Identity {
  def mails: Iterable[String] =
    Iterable(
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
      "gfxguy@sbcglobal.net"
    )
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
case class Token(uuid: UUID)

object Token {
  def unique: Token = {
    Token(UUID.randomUUID)
  }
}

package pgp

import java.util.UUID

sealed trait Identity
sealed trait KeyId
sealed trait Fingerprint

sealed trait Key {
  def keyid: KeyId
  def fingerprint: Fingerprint
  def identities: Set[Identity]
}

case class EMail(message: String, fingerprint: Fingerprint, token: Token)


// Uses type 4 UUIDs with 122 bits of strong randomness.
// Proposed by: https://github.com/wadoon/keyserver-java/
case class Token(uuid: UUID)

object Token {
  def unique: Token = {
    Token(UUID.randomUUID)
  }
}
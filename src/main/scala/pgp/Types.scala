package pgp

import scala.util.Random

sealed trait KeyId
sealed trait Fingerprint
sealed trait EMail

sealed trait Key {
  def keyid: KeyId
  def fingerprint: Fingerprint
  def emails: Set[EMail]
}

case class Token(number: BigInt)

object Token {
  def Bits = 64

  def unique: Token = {
    Token(BigInt(Bits, Random))
  }
}
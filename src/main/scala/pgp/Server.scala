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

class Server(notify: (EMail, Token) => Unit) {
  var keys: Map[Fingerprint, Key] = Map()
  var uploaded: Map[Token, Fingerprint] = Map()
  var pending: Map[Token, (Fingerprint, EMail)] = Map()
  var confirmed: Map[EMail, Fingerprint] = Map()
  var managed: Map[Token, Fingerprint] = Map()

  /**
   * TODO:
   * - rate limit requests for verification
   * - rate limit requests for management links
   * - expire management links
   */

  /**
   * Consistency invariants on the state space:
   *
   * - A key is valid if its fingerprint is registered in keys
   *
   * - All keys must be registered via the correct fingerprint
   * - Upload tokens must be for valid keys
   *
   * - Pending validations must be for valid keys
   * - Pending validations must be for email addresses associated for the respective key
   *
   * - Confirmed email addresses must be for valid keys
   * - Confirmed email must be valid for the associated key
   *
   * - Management tokens must be for valid keys
   */
  def invariants() {
    for ((fingerprint, key) <- keys) {
      assert(key.fingerprint == fingerprint)
    }

    for ((token, fingerprint) <- uploaded) {
      assert(keys contains fingerprint)
    }

    for ((token, (fingerprint, email)) <- pending) {
      assert(keys contains fingerprint)
      val key = keys(fingerprint)
      assert(key.emails contains email)
    }

    for ((email, fingerprint) <- confirmed) {
      assert(keys contains fingerprint)
      val key = keys(fingerprint)
      assert(key.emails contains email)
    }

    for ((token, fingerprint) <- managed) {
      assert(keys contains fingerprint)
    }
  }

  /**
   * Look up a key by its (unique) fingerprint.
   *
   */
  def byFingerprint(fingerprint: Fingerprint): Option[Key] = {
    keys get fingerprint
  }

  /**
   * Loop up keys by id.
   *
   * Note that the key id is not assumed to be unique.
   */
  def byKeyId(keyid: KeyId): Iterable[Key] = {
    for ((fingerprint, key) <- keys if key.keyid == keyid)
      yield key
  }

  /**
   * Look up the (unique) key associated with an email address.
   *
   * Note that this key should be in keys, too.
   */
  def byEmail(email: EMail): Option[Key] = {
    for (fingerprint <- confirmed get email)
      yield keys(fingerprint)
  }

  /**
   * Upload a new key to the server.
   *
   * The returned token must be used to requestVerify,
   * to prevent spamming users with such requests.
   *
   * Note the check for fingerprint collisions.
   */
  def upload(key: Key): Token = {
    val fingerprint = key.fingerprint
    if (keys contains fingerprint)
      assert(keys(fingerprint) == key)

    val token = Token.unique
    keys += (fingerprint -> key)
    uploaded += (token -> fingerprint)
    token
  }

  /**
   * Verify an email address by a token received via email.
   *
   * Note that we keep the mapping in uploaded to allow further verifications.
   */
  def verify(token: Token) {
    if (pending contains token) {
      val (fingerprint, email) = pending(token)
      pending -= token
      confirmed += (email -> fingerprint)
    }
  }

  /**
   * Request to verify a set of email addresses, given a token returned by upload.
   *
   * For each email address that can be verified with this token,
   * create a unique token that can later be passed to verify.
   */
  def requestVerify(from: Token, emails: Set[EMail]) {
    if (uploaded contains from) {
      val fingerprint = uploaded(from)
      val key = keys(fingerprint)
      if (emails subsetOf key.emails) {
        for (email <- emails) {
          val token = Token.unique
          pending += (token -> (fingerprint, email))
          notify(email, token)
        }
      }
    }
  }

  /**
   * Request a management token for a given confirmed email.
   *
   * Note that this should be rate-limited.
   */
  def requestManage(email: EMail) {
    if (confirmed contains email) {
      val token = Token.unique
      val key = confirmed(email)
      managed += (token -> key)
      notify(email, token)
    }
  }

  /**
   * Revoke confirmation of a set of emails given a management key.
   *
   * Only if all addresses match the respective key, they will be invalidated.
   */
  def revoke(token: Token, emails: Set[EMail]) {
    if (managed contains token) {
      val fingerprint = managed(token)
      val key = keys(fingerprint)
      if (emails subsetOf key.emails) {
        confirmed --= emails
      }
    }
  }
}

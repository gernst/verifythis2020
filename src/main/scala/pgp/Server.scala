package pgp

/**
 * Abstract model of the keyserver running at https://keys.openpgp.org/
 */
class Server(notify: (Identity, EMail) => Unit) extends Spec1 {
  var keys: Map[Fingerprint, Key] = Map()
  var uploaded: Map[Token, Fingerprint] = Map()
  var pending: Map[Token, (Fingerprint, Identity)] = Map()
  var confirmed: Map[Identity, Fingerprint] = Map()
  var managed: Map[Token, Fingerprint] = Map()

  /**
   * TODO:
   * 
   * Information flow
   * - strip unconfirmed email addresses from keys!
   * 
   * Denial of service
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
   * - Pending validations must be for identity addresses associated for the respective key
   *
   * - Confirmed identity addresses must be for valid keys
   * - Confirmed identity must be valid for the associated key
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

    for ((token, (fingerprint, identity)) <- pending) {
      assert(keys contains fingerprint)
      val key = keys(fingerprint)
      assert(key.identities contains identity)
    }

    for ((identity, fingerprint) <- confirmed) {
      assert(keys contains fingerprint)
      val key = keys(fingerprint)
      assert(key.identities contains identity)
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
   * Loop up keys by identity.
   *
   * Note that the key identity is not assumed to be unique.
   */
  def byKeyId(keyid: KeyId): Iterable[Key] = {
    for ((fingerprint, key) <- keys if key.keyid == keyid)
      yield key
  }

  /**
   * Look up the (unique) key associated with an identity address.
   *
   * Note that this key should be in keys, too.
   */
  def byEmail(identity: Identity): Option[Key] = {
    for (fingerprint <- confirmed get identity)
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
   * Request to verify a set of identity addresses, given a token returned by upload.
   *
   * For each identity address that can be verified with this token,
   * create a unique token that can later be passed to verify.
   */
  def requestVerify(from: Token, identities: Set[Identity]) {
    if (uploaded contains from) {
      val fingerprint = uploaded(from)
      val key = keys(fingerprint)
      if (identities subsetOf key.identities) {
        for (identity <- identities) {
          val token = Token.unique
          pending += (token -> (fingerprint, identity))
          val email = EMail("verify", fingerprint, token)
          notify(identity, email)
        }
      }
    }
  }

  /**
   * Verify an identity address by a token received via identity.
   *
   * Note that we keep the mapping in uploaded to allow further verifications.
   */
  def verify(token: Token) {
    if (pending contains token) {
      val (fingerprint, identity) = pending(token)
      pending -= token
      confirmed += (identity -> fingerprint)
    }
  }

  /**
   * Request a management token for a given confirmed identity.
   *
   * Note that this should be rate-limited.
   */
  def requestManage(identity: Identity) {
    if (confirmed contains identity) {
      val token = Token.unique
      val fingerprint = confirmed(identity)
      managed += (token -> fingerprint)
      val email = EMail("manage", fingerprint, token)
      notify(identity, email)
    }
  }

  /**
   * Revoke confirmation of a set of identities given a management key.
   *
   * Only if all addresses match the respective key, they will be invalidated.
   */
  def revoke(token: Token, identities: Set[Identity]) {
    if (managed contains token) {
      val fingerprint = managed(token)
      val key = keys(fingerprint)
      if (identities subsetOf key.identities) {
        confirmed --= identities
      }
    }
  }
}

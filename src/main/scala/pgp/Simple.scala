package pgp

/**
 * Very simple abstract model of a keyserver that validates additions and removal of keys.
 *
 * Note that this model is likely *wrong* and has races between adding and deleting keys
 * that make the behavior unpredictable, and possibly insecure.
 */
class Simple(notify: (Identity, EMail) => Unit) extends Spec0 {
  var keys: Map[Identity, Key] = Map()
  var add: Map[Token, (Identity, Key)] = Map()
  var del: Map[Token, (Identity, Key)] = Map()

  def byEmail(identity: Identity): Option[Key] = {
    keys get identity
  }

  def requestAdd(identity: Identity, key: Key) {
    val token = Token.unique
    add += (token -> (identity, key))
    val fingerprint = key.fingerprint
    val email = EMail("verify", fingerprint, token)
    notify(identity, email)
  }

  def confirmAdd(token: Token) {
    if (add contains token) {
      val (identity, key) = add(token)
      keys += (identity -> key)
    }
  }

  def requestDel(identity: Identity, key: Key) {
    val token = Token.unique
    del += (token -> (identity, key))
    val fingerprint = key.fingerprint
    val email = EMail("revoke", fingerprint, token)
    notify(identity, email)
  }

  def confirmDel(token: Token) {
    if (del contains token) {
      val (identity, key) = del(token)
      if ((keys get identity) == Some(key)) {
        keys -= identity
      }
    }
  }
}
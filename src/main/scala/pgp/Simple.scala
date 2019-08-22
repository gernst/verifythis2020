package pgp

/**
 * Very simple abstract model of a keyserver that validates additions and removal of keys.
 *
 * Note that this model is likely *wrong* and has races between adding and deleting keys
 * that make the behavior unpredictable, and possibly insecure.
 */
class Simple(notify: (EMail, Token) => Unit) {
  var keys: Map[EMail, Key] = Map()
  var add: Map[Token, (EMail, Key)] = Map()
  var del: Map[Token, (EMail, Key)] = Map()

  def byEmail(email: EMail): Option[Key] = {
    keys get email
  }

  def requestAdd(email: EMail, key: Key): Token = {
    val token = Token.unique
    add += (token -> (email, key))
    notify(email, token)
    token
  }

  def confirmAdd(token: Token) {
    if (add contains token) {
      val (email, key) = add(token)
      keys += (email -> key)
    }
  }

  def requestDel(email: EMail, key: Key): Token = {
    val token = Token.unique
    del += (token -> (email, key))
    notify(email, token)
    token
  }

  def confirmDel(token: Token) {
    if (del contains token) {
      val (email, key) = del(token)
      if ((keys get email) == Some(key)) {
        keys -= email
      }
    }
  }
}
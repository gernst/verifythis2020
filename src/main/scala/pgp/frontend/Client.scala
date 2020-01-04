package pgp

import types.{Fingerprint,Key,Identity,EMail}

/**
 * Abstract model of the behavior of a client of the keyserver,
 * i.e., someone holding a set of keys.
 */
class Client {
  var keys: Map[Fingerprint, Key] = Map()

  def receive(identity: Identity, email: EMail) {

  }
}
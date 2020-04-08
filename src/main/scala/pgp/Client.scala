package pgp

class Client(val identities: Set[Identity]) {
  var keys: Map[Fingerprint, Key] = Map()
  var uploaded: Map[Token, Key] = Map()
  var confirmed: Map[Identity, Fingerprint] = Map()
  var received: Map[Fingerprint, Key] = Map()
  var requested: Map[Identity, Fingerprint] = Map()
}

object Client {
  def apply(identities: Set[Identity], keys: Set[Key]): Client = {
    val client = new Client(identities)
    client.keys = keys.map(k => (k.fingerprint, k)).toMap

    client
  }
}

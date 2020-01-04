package pgp

import types._

/**
  * Abstract model of the behavior of a client of the keyserver,
  * i.e., someone holding a set of keys.
  */
class Client(
    private val in: Recv[ServerMessage],
    private val out: Send[ClientMessage],
    private val identities: Set[Identity]
) {
  var keys: Map[Fingerprint, Key] = Map()

  def receive(identity: Identity, email: EMail) = {}

  def upload(key: Key): Unit = out ! Upload(key)

  def getByKeyId(keyId: KeyId) = out ! ByKeyId(keyId)

  def getByEmail(identity: Identity) = out ! ByEmail(identity)

  def getByFingerprint(fingerprint: Fingerprint) = out ! ByFingerprint(fingerprint)
}

class ClientActor(private val client: Client)
    extends Actor[ServerMessage, ClientMessage] {
  def run(in: Recv[ServerMessage], out: Send[ClientMessage]) {

  }
}

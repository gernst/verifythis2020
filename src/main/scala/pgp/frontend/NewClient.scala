package pgp.frontend

import pgp.types._

abstract class ClientUploadKeyActor(client: NewClient)
  extends Actor[ServerMessage, ClientMessage] {

}

class NewClient(identities: Set[Identity]) {
  var keys: Map[Fingerprint, Key] = Map()
}
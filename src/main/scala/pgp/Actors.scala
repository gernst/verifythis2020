package pgp



class ServerActor(server: Spec1) extends PassiveActor {
  def handle(from: Actor, msg: Message): Unit = msg match {
    case ByEmail(identity) =>
      send(from, FromEmail(server byEmail identity))
    case ByFingerprint(fingerprint) =>
      send(from, FromFingerprint(server byFingerprint fingerprint))
    case ByKeyId(keyId) =>
      send(from, FromKeyId(server byKeyId keyId))
    case Upload(key) =>
      send(from, Uploaded(server.upload(key)))
    case RequestVerify(tok, identity) =>
      for (Body(f, t, identity) <- server.requestVerify(tok, Set(identity))) {
        send(identity, Body(f, t, identity))
      }

    case Verify(token) => server.verify(token)
    case RequestManage(identity) =>
      for (mail <- server.requestManage(identity)) {
        send(from, Manage(mail))
      }
    case Revoke(token, identities) =>
      server.revoke(token, identities)
    case _ =>
  }
  // server.invariants()

  def handle(from: Actor, msg: Body): Unit = {}
}

class UploadActor(client: Client, key: Key, server: ServerActor) extends Actor {
  var canAct = true

  def act() = {
    send(server, Upload(key))
    canAct = false
  }

  def handle(from: Actor, msg: Message): Unit = msg match {
    case Uploaded(token) => client.uploaded += (token -> key)
  }

  def handle(from: Actor, msg: Body): Unit = {}
}

class ByMailActor(client: Client, id: Identity, server: ServerActor) extends Actor {
  def canAct = {
    client.confirmed contains id
  }

  def act() = {
    send(server, ByEmail(id))
  }

  def handle(from: Actor, msg: Message): Unit = msg match {
    case Init =>
    case FromEmail(optKey) =>
      for (key <- optKey) {
        client.received += (key.fingerprint -> key)
      }
  }

  def handle(from: Actor, msg: Body): Unit = {}
}

class VerifyActor(client: Client, id: Identity, server: ServerActor) extends Actor {
  register(id)

  def canAct = {
    client.uploaded exists {
      case (_, key) => key.identities contains id
    }
  }

  def act() {
    val Some((token, key)) = client.uploaded find {
      case (_, key) => key.identities contains id
    }

    send(server, RequestVerify(token, id))
  }

  def handle(from: Actor, msg: Message): Unit = {
  }

  override def handle(from: Actor, msg: Body): Unit = {
    val Body(f, token, identity) = msg
    send(from, Verify(token))
    client.confirmed += (identity -> f)
  }
}

class RevokeActor(client: Client, id: Identity, server: ServerActor) extends Actor {

  def canAct: Boolean = {
    client.confirmed contains id
  }


}

class UploadNotValidatedKeySpec(client: Client, server: ServerActor)
  extends TestSpec {

  def prepare(): Unit = {
    val keys = for (identity <- client.identities.sliding(2, 2))
      yield Key.random(identity)

    client.keys = keys.map(k => (k.fingerprint, k)).toMap
  }

  def andThen(next: TestSpec): Boolean = {
    val result = run()

    result & next.run()

  }

  def run(): Boolean = ???
}

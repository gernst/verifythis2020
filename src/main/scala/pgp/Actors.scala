package pgp

import scala.collection.immutable.Queue
import scala.collection.mutable

class ServerActor(server: Spec1) extends Actor {
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

class UploadActor(client: Client, key: Key) extends Actor {

  def handle(from: Actor, msg: Message): Unit = msg match {
    case Init => send(from, Upload(key))
    case Uploaded(token) => client.uploaded += (token -> key)
    case _ =>
  }

  override def handle(from: Actor, msg: Body): Unit = {}
}

class ByMailActor(client: Client) extends Actor {

  def handle(from: Actor, msg: Message): Unit = msg match {
    case Init =>
      val id = client.confirmed.head._1
      send(from, ByEmail(id))
    case FromEmail(optKey) =>
      for (key <- optKey) {
        client.received += (key.fingerprint -> key)
      }

    case _ =>
  }

  override def handle(from: Actor, msg: Body): Unit = {}
}

class VerifyActor(client: Client) extends Actor {

  var identity: Identity = _

  def handle(from: Actor, msg: Message): Unit = msg match {
    case Init =>
      for (
        (token, id) <- client.uploaded.headOption.map {
          case (tok, k) => (tok, k.identities.head)
        }
      ) {
        register(id)
        send(from, RequestVerify(token, id))
        identity = id
      }
    case _ =>
  }

  override def handle(from: Actor, msg: Body): Unit = {
    val Body(f, token, identity) = msg
    send(from, Verify(token))
    client.confirmed += (identity -> f)
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

  def run(): Boolean = {

    val seq = pgp.c(0)
    val (_, key) = client.keys.head
    val uploadActor = new UploadActor(client, key)

    val verifyActor = new VerifyActor(client)

    val downloadActor = new ByMailActor(client)

    //    uploadActor handle (server, Init)
    //
    //    verifyActor handle (server, Init)
    //
    //    downloadActor handle (server, Init)

    Network.sequence(server, uploadActor, verifyActor, downloadActor)

    // while (network.step(seq)) {}

    println(s"Uploaded: ${client.uploaded.size}")
    println(s"Confirmed: ${client.confirmed.size}")
    println(s"Requested ${client.received.size}")
    client.received.forall { case (_, key) => key.identities.size == 1 }

  }
}

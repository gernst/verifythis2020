package pgp.frontend

import pgp.backend.ServerActor
import pgp.types._

import scala.collection.immutable.Queue

class UploadActor(client: NewClient, key: Key) extends Actor {

  def handle(from: Actor, msg: Message): Unit = msg match {
    case Init => send(from, Upload(key))
    case Uploaded(token) => client.uploaded += (token -> key)
    case _ =>
  }

  override def handle(from: Actor, msg: Body): Unit = {}
}

//class ClientUploadKeyActor(client: NewClient,
//                           connection: Connection[ClientMessage, ServerMessage],
//                           key: Key)
//  extends Actor {
//
//  override def state: ActorState = if (uploaded) Finished else Running
//
//  var uploaded = false
//  var requested = false
//
//  def step(rnd: Iterator[Int]): Unit = {
//    if (!requested) {
//      connection.send ! Upload(key)
//      requested = true
//    }
//    if (connection.recv.canRecv) {
//      handle(connection.recv(), connection.send)
//    }
//
//  }
//
//  def handle(msg: ServerMessage, out: Send[ClientMessage]): Unit = {
//    msg match {
//      case Uploaded(token) =>
//        client.uploaded += (token -> key)
//        uploaded = true
//      case _ =>
//    }
//  }
//
//}

class ByMailActor(client: NewClient) extends Actor {

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

class VerifyActor(client: NewClient) extends Actor {

  var identity: Identity = _

  def handle(from: Actor, msg: Message): Unit = msg match {
    case Init =>
      for ((token, id) <- client.uploaded.headOption.map {
        case (tok, k) => (tok, k.identities.head)
      }) {
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

class AttackActor extends Attacker {

  def inspect(state: Queue[Data]): Unit = for (msg <- state) {
    println(msg)
  }

}

class UploadNotValidatedKeySpec(client: NewClient, server: ServerActor)
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

  def run()(implicit network: Network): Boolean = {

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

    network.sequence(server, uploadActor, verifyActor, downloadActor)

    // while (network.step(seq)) {}

    println(s"Uploaded: ${client.uploaded.size}")
    println(s"Confirmed: ${client.confirmed.size}")
    println(s"Requested ${client.received.size}")
    client.received.forall { case (_, key) => key.identities.size == 1 }

  }
}

class NewClient(val identities: Set[Identity]) {
  var keys: Map[Fingerprint, Key] = Map()
  var uploaded: Map[Token, Key] = Map()
  var confirmed: Map[Identity, Fingerprint] = Map()
  var received: Map[Fingerprint, Key] = Map()
  var requested: Map[Identity, Fingerprint] = Map()
}

object NewClient {
  def apply(identities: Set[Identity], keys: Set[Key]): NewClient = {
    val client = new NewClient(identities)
    client.keys = keys.map(k => (k.fingerprint, k)).toMap

    client
  }
}

object Test {

  def test(): Unit = {}
}

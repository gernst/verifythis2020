package pgp.frontend

import pgp.TestSpec
import pgp.backend.ServerActor
import pgp.types._

class UploadActor(client: NewClient, key: Key) extends Actor {

  def handle(from: Actor, msg: Message): Unit = msg match {
    case Init => send(from, Upload(key))
    case Uploaded(token) => client.uploaded += (token -> key)
    case _ =>
  }

  override def handle(from: Actor, msg: Body): Unit = ???
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

  override def handle(from: Actor, msg: Body): Unit = ???
}

//class ClientByMailActor(client: NewClient,
//                        connection: Connection[ClientMessage, ServerMessage],
//) extends Actor {
//
//  var key: Option[Key] = None
//  var requested = false
//  var received = false
//
//  override def state: ActorState = if (received) Finished else Running
//
//  def step(rnd: Iterator[Int]): Unit = {
//    if (!requested) {
//      for (identity <- chooseIdentity()) {
//        connection.send ! ByEmail(identity)
//        requested = true
//      }
//
//    } else {
//      if (connection.recv.canRecv) {
//        val msg = connection.recv()
//        handle(msg)
//      }
//    }
//  }
//
//  def handle(msg: ServerMessage): Unit = {
//    msg match {
//      case FromEmail(optKey) =>
//        for (key <- optKey) {
//          client.received += (key.fingerprint -> key)
//        }
//        received = true
//      case _ =>
//    }
//  }
//
//  private def chooseIdentity(): Option[Identity] =
//    if (client.confirmed.isEmpty) {
//      None
//    } else {
//      val selection = Random.nextInt(client.confirmed.size)
//      val iter = client.confirmed.iterator
//        .filterNot { case (id, _) => client.requested.contains(id) }
//
//      var opt: Option[Identity] = None
//
//      if (iter.hasNext) {
//        val (id, f) = iter.next()
//        opt = Some(id)
//        client.requested += (id -> f)
//      } else {
//        opt = None
//      }
//
//      opt
//
//    }
//
//}

class VerifyActor(client: NewClient) extends Actor {

  var identity: Identity = _


  def handle(from: Actor, msg: Message): Unit = msg match {
    case Init =>
      for ((token, id) <- client.uploaded.headOption.map { case (tok, k) => (tok, k.identities.head) }) {
        send(from, RequestVerify(token, id))
        identity = id
      }
    case Verification(email) =>
      send(from, Verify(email.token))
      client.confirmed += (identity -> email.fingerprint)

    case _ =>
  }

  override def handle(from: Actor, msg: Body): Unit = ???
}


//class ClientVerifyActor(client: NewClient,
//                        connection: Connection[ClientMessage, ServerMessage],
//) extends Actor {
//
//  var requested = false
//  var confirmed = false
//  var toVerify: Option[Identity] = None
//  var identity: Identity = _
//
//  override def state: ActorState = if (confirmed) Finished else Running
//
//  def handle(msg: ServerMessage, out: Send[ClientMessage]): Unit = {
//    msg match {
//      case Verification(email) =>
//        out ! Verify(email.token)
//        confirmed = true
//        client.confirmed += (identity -> email.fingerprint)
//      case _ =>
//    }
//  }
//
//  def step(rnd: Iterator[Int]): Unit = {
//    if (!requested) {
//      for ((token, id) <- chooseIdentity()) {
//        identity = id
//        connection.send ! RequestVerify(token, id)
//        requested = true
//      }
//
//    } else {
//      if (connection.recv.canRecv) {
//        val msg = connection.recv()
//        handle(msg, connection.send)
//      }
//    }
//  }
//
//  private def chooseIdentity() =
//    if (client.uploaded.isEmpty) {
//      None
//    } else {
//      val selection = Random.nextInt(client.uploaded.size)
//      val (token, key) = client.uploaded.iterator.drop(selection).next
//
//      client.uploaded -= token
//      Some(token, key.identities.head)
//    }
//
//
//
//}

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

    //    val uploadActors =
    //      (for ((_, key) <- client.keys; connection = server.connect())
    //        yield new ClientUploadKeyActor(client, connection, key)).toList
    //
    //    val verifyActors =
    //      (for (_ <- client.keys; connection = server.connect())
    //        yield new ClientVerifyActor(client, connection)).toList
    //
    //    val downloadActors =
    //      (for (identity <- client.identities; connection = server.connect())
    //        yield new ClientByMailActor(client, connection)).toList
    //
    //    val combined = (uploadActors ::: verifyActors ::: downloadActors) ::: List(
    //      server
    //    )
    //
    //    val actorSeq: Actor = Actor.sequence(combined)
    //
    //    val rnd = pgp.r()
    //
    //    while (actorSeq.state != Finished) {
    //      actorSeq.step(rnd)
    //    }


    val seq = pgp.c(0)
    val (fingerprint, key) = client.keys.head
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

object Test {

  def test(): Unit = {}
}

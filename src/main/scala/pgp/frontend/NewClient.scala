package pgp.frontend

import pgp.backend.ServerActor
import pgp.types._
import pgp.{ActorState, Finished, Running, TestSpec}

class ClientUploadKeyActor(client: NewClient,
                           connection: Connection[ClientMessage, ServerMessage],
                           key: Key)
  extends Actor {

  override def state: ActorState = if (uploaded) Finished else Running

  var uploaded = false
  var requested = false

  def step(rnd: Iterator[Int]): Unit = {
    if (!requested) {
      connection.send ! Upload(key)
      requested = true
    }
    if (connection.recv.canRecv) {
      handle(connection.recv(), connection.send)
    }

  }

  def handle(msg: ServerMessage, out: Send[ClientMessage]): Unit = {
    msg match {
      case Uploaded(token) =>
        client.uploaded += (token -> key)
        uploaded = true
      case _ =>
    }
  }

}

class ClientByMailActor(client: NewClient,
                        connection: Connection[ClientMessage, ServerMessage],
                        identity: Identity)
  extends Actor {

  var key: Option[Key] = None
  var requested = false
  var received = false

  override def state: ActorState = if (received) Finished else Running

  def step(rnd: Iterator[Int]): Unit = {
    if (!requested) {
      connection.send ! ByEmail(identity)
      requested = true
    }
    if (connection.recv.canRecv) {
      val msg = connection.recv()
      handle(msg)
    }
  }

  def handle(msg: ServerMessage): Unit = {
    msg match {
      case FromEmail(optKey) =>
        for (key <- optKey) {
          client.requested += (key.fingerprint -> key)
        }
        received = true
      case _ =>
    }
  }

}

class ClientVerifyActor(client: NewClient,
                        connection: Connection[ClientMessage, ServerMessage],
                        token: Token,
                        identity: Identity)
  extends Actor {

  var requested = false
  var confirmed = false
  var toVerify: Option[Identity] = None

  override def state: ActorState = if (confirmed) Finished else Running

  def step(rnd: Iterator[Int]): Unit = {
    if (!requested) {
      connection.send ! RequestVerify(token, identity)
      requested = true
    } else {
      if (connection.recv.canRecv) {
        val msg = connection.recv()
        handle(msg, connection.send)
      }
    }
  }

  def handle(msg: ServerMessage, out: Send[ClientMessage]): Unit = {
    msg match {
      case Verification(email) =>
        out ! Verify(email.token)
        confirmed = true
        client.confirmed += (identity -> email.fingerprint)
      case _ =>
    }
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

  def run(): Boolean = {


    val rnd = pgp.r()

    // upload all keys

    val uploadActors =
      (for ((_, key) <- client.keys; connection = server.connect())
        yield new ClientUploadKeyActor(client, connection, key)).toSeq

    while (!uploadActors.forall(_.state == Finished)) {
      uploadActors.foreach(_.step(rnd))
      server.step(rnd)
    }

    val verifyActors =
      (for ((token, key) <- client.uploaded; id = key.identities.head;
            connection = server.connect())
        yield new ClientVerifyActor(client, connection, token, id)).toSeq

    while (!verifyActors.forall(_.state == Finished)) {

      verifyActors.foreach(_.step(rnd))
      server.step(rnd)
    }


    val downloadActors =
      (for (identity <- client.identities; connection = server.connect())
        yield new ClientByMailActor(client, connection, identity)).toSeq


    while (!downloadActors.forall(_.state == Finished)) {

      downloadActors.foreach(_.step(rnd))
      server.step(rnd)
    }


    client.requested.forall { case (_, key) => key.identities.size == 1 }

  }
}

class NewClient(val identities: Set[Identity]) {
  var keys: Map[Fingerprint, Key] = Map()
  var uploaded: Map[Token, Key] = Map()
  var confirmed: Map[Identity, Fingerprint] = Map()
  var requested: Map[Fingerprint, Key] = Map()
}

object Test {

  def test(): Unit = {}
}

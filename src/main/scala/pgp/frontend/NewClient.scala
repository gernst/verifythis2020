package pgp.frontend

import pgp.types._

class ClientUploadKeyActor(client: NewClient,
                           connection: Connection[ClientMessage, ServerMessage])
  extends Actor[ServerMessage, ClientMessage] {

  var key: Option[Key] = None

  def step(rnd: Iterator[Int]): Unit = {
    key match {
      case Some(_) =>
        if (connection.recv.canRecv) {
          handle(connection.recv(), connection.send)
        }
      case None =>
    }
  }

  def handle(msg: ServerMessage, out: Send[ClientMessage]): Unit = {
    msg match {
      case Uploaded(token) =>
        key match {
          case Some(key) => client.uploaded + (token -> key.fingerprint)
          case None =>
        }
      case _ =>
    }
  }

  def upload(k: Key): Unit = {
    key = Some(k)
  }

}

class ClientByMailActor(client: NewClient,
                        connection: Connection[ClientMessage, ServerMessage])
  extends Actor[ServerMessage, ClientMessage] {

  var key: Option[Key] = None
  var id: Option[Identity] = None
  var requested = false

  // TODO: what should happen with keys that the client requested ?
  def step(rnd: Iterator[Int]): Unit = id match {
    case Some(identity) =>
      if (!requested) {
        connection.send ! ByEmail(identity)
      }
      if (connection.recv.canRecv) {
        val msg = connection.recv()
        handle(msg)
      }

    case _ =>
  }

  def handle(msg: ServerMessage): Unit = {
    msg match {
      case FromEmail(optKey) => key = optKey
      case _ =>
    }
  }

  def get(identity: Identity): Unit = {
    id = Some(identity)
  }

}

class ClientVerifyActor(client: NewClient,
                        connection: Connection[ClientMessage, ServerMessage])
  extends Actor[ServerMessage, ClientMessage] {

  var toVerify: Option[(Token, Set[Identity])] = None
  var requested = false

  def step(rnd: Iterator[Int]): Unit = {
    toVerify match {
      case Some((token, identities)) =>
        if (!requested) {
          connection.send ! RequestVerify(token, identities)
        }
        if (connection.recv.canRecv) {
          val msg = connection.recv()
          handle(msg, connection.send)
        }

      case _ =>
    }
  }

  def handle(msg: ServerMessage, out: Send[ClientMessage]): Unit = {
    msg match {
      case Verification(email) => ???
      case _ =>
    }
  }

  def verify(token: Token, identities: Set[Identity]): Unit = {
    toVerify = Some(token, identities)
  }


}

class NewClient(identities: Set[Identity]) {
  var keys: Map[Fingerprint, Key] = Map()
  var uploaded: Map[Token, Fingerprint] = Map()
  var confirmed: Map[Token, Fingerprint] = Map()
}
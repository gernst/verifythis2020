package pgp.types


import scala.collection.immutable.Queue
import scala.collection.mutable

trait Send[A] {
  def send(a: A)

  def !(a: A): Unit = send(a)
}

trait Recv[A] {
  def canRecv: Boolean
  def recv: A

  def apply(): A
}

trait Attacker extends Actor {

  def inspect(state: Queue[Data])

  def getState(implicit network: Network): Queue[Data] = network.currentState

  override def handle(from: Actor, msg: Message): Unit = inspect(getState)

  /**
   * The attacker can inspect the current state of all packages but has no access to mails that
   * are currently in the network
   **/
  def handle(from: Actor, msg: Body): Unit = {}
}


trait Actor {
  //  def state: ActorState
  //  // def run(in: Recv[In], out: Send[Out]): scala.concurrent.Future[Unit]
  //  def step(rnd: Iterator[Int]): Unit
  def handle(from: Actor, msg: Message)

  def handle(from: Actor, msg: Body)

  def send(to: Actor, msg: Message)(implicit network: Network): Unit = network.send(this, to, msg)

  def send(to: Identity, msg: Body)(implicit network: Network): Unit = network.send(this, to, msg)

  def register(identity: Identity)(implicit network: Network): Unit = network.register(identity, this)
}

trait Network {
  def send(from: Actor, to: Actor, msg: Message)

  def send(from: Actor, to: Identity, msg: Body)

  def step(rnd: Iterator[Int]): Boolean

  def sequence(server: Actor, actors: Actor*): Unit

  def register(identity: Identity, actor: Actor): Unit

  def currentState: Queue[Data]


}

object Network {

  implicit object NetworkImpl extends Network with Actor {
    var msgs: Queue[Data] = Queue()

    val mailHandler: mutable.Map[Identity, Actor] = scala.collection.mutable.Map()

    def currentState: Queue[Data] = msgs.filter(_.isInstanceOf[Packet])

    def register(identity: Identity, actor: Actor): Unit = mailHandler += (identity -> actor)

    def state: ActorState = Running

    def send(from: Actor, to: Actor, msg: Message) {
      val pkt = Packet(from, to, msg)
      msgs = msgs enqueue pkt
    }

    def send(from: Actor, to: Identity, msg: Body): Unit = {
      val mail = Mail(from, to, msg)
      msgs = msgs enqueue mail
    }

    // def run(in: Recv[In], out: Send[Out]): scala.concurrent.Future[Unit]
    def step(rnd: Iterator[Int]): Boolean = {
      if (msgs.nonEmpty) msgs.dequeue match {
        case (Mail(from, to, msg), queue) =>
          msgs = queue
          mailHandler.get(to) foreach (_ handle(from, msg))
        case (Packet(from, to, msg), queue) =>
          msgs = queue
          to handle(from, msg)
      }

      msgs.nonEmpty
    }

    def sequence(server: Actor, actors: Actor*): Unit = {

      for (current <- actors) {
        current handle(server, Init)

        while (step(pgp.c(0))) {}
      }
    }


    def handle(from: Actor, msg: Message): Unit = {}

    def handle(from: Actor, msg: Body): Unit = {}

  }
}


case class Channel[A](send: Send[A], recv: Recv[A])
case class Connection[Out, In](send: Send[Out], recv: Recv[In])

object Channel {
  def queue[A]: Channel[A] = {

    object ch extends Send[A] with Recv[A] {
      val msgs: mutable.Queue[A] = scala.collection.mutable.Queue[A]()

      def send(a: A) { msgs enqueue a }

      def canRecv: Boolean = msgs.nonEmpty

      def recv: A = msgs.dequeue

      def apply(): A = recv
    }

    Channel(ch, ch)
  }

}

final case class Body(fingerprint: Fingerprint, token: Token, identity: Identity)

sealed trait Data

case class Mail(from: Actor, to: Identity, body: Body) extends Data

case class Packet(from: Actor, to: Actor, msg: Message) extends Data

sealed trait Message

object Init extends Message

sealed trait ServerMessage extends Message

final case class Manage(email: EMail) extends ServerMessage

final case class Verification(email: EMail) extends ServerMessage

final case class FromFingerprint(key: Option[Key]) extends ServerMessage

final case class FromKeyId(key: Iterable[Key]) extends ServerMessage

final case class FromEmail(key: Option[Key]) extends ServerMessage

final case class Uploaded(token: Token) extends ServerMessage

sealed trait ClientMessage extends Message

final case class Upload(key: Key) extends ClientMessage

final case class ByFingerprint(fingerprint: Fingerprint) extends ClientMessage

final case class ByKeyId(keyId: KeyId) extends ClientMessage

final case class ByEmail(identity: Identity) extends ClientMessage

final case class RequestVerify(from: Token, identity: Identity) extends ClientMessage

final case class Verify(token: Token) extends ClientMessage

final case class RequestManage(identity: Identity) extends ClientMessage

final case class Revoke(token: Token, identities: Set[Identity]) extends ClientMessage
package pgp.types

import pgp.{ActorState, Finished, Running}
import pgp.backend.ServerActor

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

trait Actor {
  def state: ActorState
  // def run(in: Recv[In], out: Send[Out]): scala.concurrent.Future[Unit]
  def step(rnd: Iterator[Int]): Unit
}

object Actor {
  def sequence(actors: List[Actor]): Actor = new Actor {
    var todo = actors

    def state: ActorState = if (todo.isEmpty || (todo.size == 1 && todo.head.isInstanceOf[ServerActor])) Finished else Running
    // def run(in: Recv[In], out: Send[Out]): scala.concurrent.Future[Unit]
    def step(rnd: Iterator[Int]): Unit = {
      val actor = todo.head
      actor step rnd
      if (actor.state == Finished)
        todo = todo.tail
    }
  }

  def choice(actors: List[Actor]) = new Actor {
    var todo = actors

    def state: ActorState = if (todo.isEmpty || (todo.size == 1 && todo.head.isInstanceOf[ServerActor])) Finished else Running
    def step(rnd: Iterator[Int]): Unit = {
      val actor = pgp.choose(todo, rnd)
      actor step rnd
      if (actor.state == Finished)
        todo = todo filter (_ != actor)
    }
  }
}

case class Channel[A](send: Send[A], recv: Recv[A])
case class Connection[Out, In](send: Send[Out], recv: Recv[In])

object Channel {
  def queue[A]: Channel[A] = {

    object ch extends Send[A] with Recv[A] {
      val msgs: mutable.Queue[A] = scala.collection.mutable.Queue[A]()

      def send(a: A) { msgs enqueue a }

      def canRecv: Boolean = !msgs.isEmpty

      def recv: A = msgs.dequeue

      def apply(): A = recv
    }

    Channel(ch, ch)
  }

}

sealed trait ServerMessage

final case class Manage(email: EMail) extends ServerMessage

final case class Verification(email: EMail) extends ServerMessage

final case class FromFingerprint(key: Option[Key]) extends ServerMessage

final case class FromKeyId(key: Iterable[Key]) extends ServerMessage

final case class FromEmail(key: Option[Key]) extends ServerMessage

final case class Uploaded(token: Token) extends ServerMessage

sealed trait ClientMessage

final case class Upload(key: Key) extends ClientMessage

final case class ByFingerprint(fingerprint: Fingerprint) extends ClientMessage

final case class ByKeyId(keyId: KeyId) extends ClientMessage

final case class ByEmail(identity: Identity) extends ClientMessage

final case class RequestVerify(from: Token, identity: Identity) extends ClientMessage

final case class Verify(token: Token) extends ClientMessage

final case class RequestManage(identity: Identity) extends ClientMessage

final case class Revoke(token: Token, identities: Set[Identity]) extends ClientMessage
package pgp

import java.util.UUID

sealed trait Identity
sealed trait KeyId
sealed trait Fingerprint

sealed trait Key {
  def keyid: KeyId
  def fingerprint: Fingerprint
  def identities: Set[Identity]
}

case class EMail(message: String, fingerprint: Fingerprint, token: Token)

// Uses type 4 UUIDs with 122 bits of strong randomness.
// Proposed by: https://github.com/wadoon/keyserver-java/
case class Token(uuid: UUID)

object Token {
  def unique: Token = {
    Token(UUID.randomUUID)
  }
}

trait Send[A] {
  def send(a: A)
}

trait Recv[A] {
  def recv: Option[A]
}

sealed trait Actor[In,Out] {
  def run(in: Recv[In], out: Send[Out]): Unit
}

object Channel {
  def queue[A](): (Send[A], Recv[A]) = {
    object ch extends Send[A] with Recv[A] {
      import scala.collection.mutable
      val msgs = mutable.Queue[A]()
      def send(a: A) { msgs enqueue a }
      def recv = if(msgs.isEmpty) None else Some(msgs.dequeue)
    }
    (ch, ch)
  }

  def _queue[A](): (Send[A], Recv[A]) = {
    import scala.collection.mutable
    val msgs = mutable.Queue[A]()

    object send extends Send[A] {
      def send(a: A) { msgs enqueue a }
    }

    object recv extends Recv[A] {
      def recv = if(msgs.isEmpty) None else Some(msgs.dequeue)
    }

    (send, recv)
  }

}
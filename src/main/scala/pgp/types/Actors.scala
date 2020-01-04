package pgp.types

trait Send[A] {
    def send(a: A)
  }
  
  trait Recv[A] {
    def recv: Option[A]
  }
  
  trait Actor[In,Out] {
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
  
  
  }
  
  sealed trait ServerMessage

  final case class Manage(fingerprint: Fingerprint, token: Token) extends ServerMessage

  final case class Verify(fingerprint: Fingerprint, token: Token) extends ServerMessage

  final case class FromFingerprint(key: Option[Key]) extends ServerMessage

  final case class FromKeyId(key: Iterable[Key])

  final case class FromEmail(key: Option[Key]) extends ServerMessage
  



  sealed trait ClientMessage
  
  final case class Upload(key: Key) extends ClientMessage
  
  final case class ByFingerprint(fingerprint: Fingerprint) extends ClientMessage
  
  final case class ByKeyId(keyId: KeyId) extends ClientMessage
  
  final case class ByEmail(identity: Identity) extends ClientMessage
  
  final case class RequestVerify(from: Token, identities: Set[Identity]) extends ClientMessage
  
  final case class Verify(token: Token) extends ClientMessage
  
  final case class RequestManage(identity: Identity) extends  ClientMessage

  final case class Revoke(token: Token, identities: Set[Identity]) extends ClientMessage
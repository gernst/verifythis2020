package pgp.types
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.Future

trait Send[A] {
    def send(a: A)

    def !(a: A) = send(a)
  }
  
  trait Recv[A] {
    def recv: Option[A]
  }
  
  trait Actor[In,Out] {
    def run(in: Recv[In], out: Send[Out]): scala.concurrent.Future[Unit]
  }

  
  
  object Channel {
  
    def queue[A](): (Send[A], Recv[A]) = {
  
      object ch extends Send[A] with Recv[A] {
        import  java.util.concurrent.BlockingQueue
        val msgs = new LinkedBlockingQueue[A]
  
        def send(a: A) { msgs put a }
        def recv = Some(msgs.poll())
      }
  
      (ch, ch)
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
  
  final case class RequestVerify(from: Token, identities: Set[Identity]) extends ClientMessage
  
  final case class Verify(token: Token) extends ClientMessage
  
  final case class RequestManage(identity: Identity) extends ClientMessage

  final case class Revoke(token: Token, identities: Set[Identity]) extends ClientMessage
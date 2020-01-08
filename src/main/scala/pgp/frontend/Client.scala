package pgp

import types._
import scala.concurrent.Future
import scala.collection.mutable.Map

/**
  * Abstract model of the behavior of a client of the keyserver,
  * i.e., someone holding a set of keys.
  */
class Client(
    val in: Recv[ServerMessage],
    val out: Send[ClientMessage],
    private val identities: Set[Identity]
) {

  val keys: Map[Fingerprint, Key] = Map()
  val managed: Map[Identity, Option[Token]] = Map()
  // when a key is validated, it is removed from this map and put into {keys}
  val uploaded: Map[Key, Token] = Map()

  def receive(identity: Identity, email: EMail) = {}

  def upload(key: Key): Unit = ???

  def verify(identities: Set[Identity]) = ???

  def getByKeyId(keyId: KeyId) = out ! ByKeyId(keyId)

  def getByEmail(identity: Identity) = out ! ByEmail(identity)

  def getByFingerprint(fingerprint: Fingerprint) =
    out ! ByFingerprint(fingerprint)

  def manage(id: Identity) = {
    managed(id) = None
    out ! RequestManage(id)
  }

  def revoke(identities: Set[Identity]) = {
    val fingerprint =
      keys
        .filter { case (fp, k) => identities.subsetOf(k.identities) }
        .map(_._1)
        .head

    managed
      .filter {
        case (id, Some(token)) => { identities.contains(id) }
        case (id, None) => {
          log(
            "Tried to revoke a set of identities without a management token",
            WARNING
          ); false
        }
      }
      .foreach {
        case (_, Some(token)) => out ! Revoke(token, identities)
        case _                =>
      }
  }

}

class ClientActor(private val client: Client)
    extends Actor[ServerMessage, ClientMessage] {

  implicit val ec = scala.concurrent.ExecutionContext.global

  override def run(
      in: Recv[ServerMessage] = client.in,
      out: Send[ClientMessage] = client.out
  ): Future[Unit] =
    Future {
      for (i <- Stream.continually(in.recv); el <- i) {
        el match {
          case FromEmail(Some(key))       => log("Received!")
          case FromEmail(None)            =>
          case FromFingerprint(Some(key)) => client.keys(key.fingerprint) = key
          case FromFingerprint(None)      =>
          case FromKeyId(iter) =>
            for (key <- iter) {
              client.keys(key.fingerprint) = key
            }
          // try to find the identity with wich this token is
          // associated
          case Manage(EMail(_, fingerprint, token)) => {
            val ids = client.keys
              .filter { case (fp, _) => fingerprint == fp }
              .map { case (fp, key) => key.identities }
              .head

            client.managed
              .filter { case (id, _) => ids contains id }
              .foreach { case (id, _) => client.managed(id) = Some(token) }

          }

          case Uploaded(token)                            => {}
          case Verification(EMail(_, fingerprint, token)) => out ! Verify(token)

        }
      }
    }
}

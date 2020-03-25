package pgp

import scala.collection.mutable

sealed trait Event

object Event {



  case class Revoke(ids: Set[Identity], fingerprint: Fingerprint) extends Event

  // vlt einfach Key als parameter
  case class Upload(key: Key) extends Event

  case class Verify(ids: Set[Identity], fingerprint: Fingerprint) extends Event

  case object Check extends Event

}

/**
 *
 */
sealed trait Status

case object Public extends Status

case object Private extends Status

case object Revoked extends Status

/**
 *
 */
case class History(events: mutable.Buffer[Event] = mutable.Buffer()) {
  type IdState = (Identity, Status)

  def +(event: Event): Unit = {
    events append event
  }

  /**
   * Returns the set of identities and their status given the flow of [history]
   */
  def publicIdentitiesFor(fingerprint: Fingerprint): Set[(Identity, Status)] =
    identities(fingerprint) filter (_._2 == Public)

  /**
   * Returns a Map that associates a fingerprint with its identities that were added/verified/revoked during the
   * symbolic execution of the given history. Entries are marked with a Status that describes, whether the identity is
   * public/private or has been revoked
   */
  def identities: Map[Fingerprint, Set[IdState]] = {
    val acc =
      Map[Fingerprint, Set[IdState]]().withDefaultValue(Set.empty[IdState])

    events.foldLeft(acc) { (acc, event) =>
      event match {
        case Event.Upload(key) =>
          acc + ((key.fingerprint, key.identities.map(new IdState(_, Private))))
        case Event.Revoke(ids: Set[Identity], fingerprint: Fingerprint) =>
          acc updated(fingerprint,
            acc(fingerprint) map {
              case (id, status) =>
                if (ids contains id) (id, Revoked) else (id, status)
            })
        case Event.Verify(ids, fingerprint) =>
          acc updated(fingerprint,
            acc(fingerprint) map {
              case (id, status) =>
                if (ids contains id) (id, Public) else (id, status)
            })
      }
    }
  }

  /**
   * Turn a high level history into a Seq of actors. The execution of this Seq is expected to result in the same effects
   * as the history.
   */
  def toActors(history: History,
               client: Client,
               server: ServerActor): Seq[Actor] = history.events flatMap {
    case Event.Upload(key: Key) => Seq(new UploadActor(client, key, server))
    case Event.Revoke(ids: Set[Identity], fingerprint: Fingerprint) =>
      Seq(new PassiveActor {
        override def handle(from: Actor, msg: Message): Unit = {}

        override def handle(from: Actor, msg: Body): Unit = {}
      })
    case Event.Verify(ids: Set[Identity], _) =>
      ids map (new VerifyActor(client, _, server))
  }

  /**
   * This method should return all instances at which the history and the server responses differ
   * TODO
   */
  def check(server: Server) = {

    for ((fingerprint, identities) <- identities;
         response = server byFingerprint fingerprint) {
      response match {
        case None => _
        case Some(key) => (key.identities union (identities map (_._1)))

      }
    }
  }

}

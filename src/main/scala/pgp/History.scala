package pgp

import scala.collection.{immutable, mutable}

sealed trait Event

object Event {
  case class Revoke(ids: Set[Identity], fingerprint: Fingerprint) extends Event

  case class Upload(key: Key) extends Event

  case class Verify(ids: Set[Identity], fingerprint: Fingerprint) extends Event

  case object Check extends Event

}

sealed trait Status

case object Public extends Status

case object Private extends Status

case object Revoked extends Status

sealed trait EvalResult

object EvalResult {

  case class Mismatch(fromHistory: Option[(Identity, Status)],
                      fromServer: Option[Identity])
      extends EvalResult

  case object Ok extends EvalResult

}

object History {

  implicit class Pretty[K, V](val map: Map[K, V]) {
    def prettyPrint: Pretty[K, V] = this

    override def toString: String =
      "Map (\n" + toStringLines.mkString("\n") + "\n)"

    def toStringLines: immutable.Iterable[String] =
      map
        .flatMap { case (k, v) => keyValueToString(k, v) }
        .map(indentLine)

    def keyValueToString(key: K, value: V): Iterable[String] = {
      value match {
        case v: Map[_, _] =>
          Iterable(key + " -> Map (") ++ v.prettyPrint.toStringLines ++ Iterable(
            ")"
          )
        case x => Iterable(key + " -> " + x.toString)
      }
    }

    def indentLine(line: String): String = "\t" + line
  }

  def prettyPrint(
                   checked: Map[Fingerprint, Map[Identity, EvalResult]]
                 ): String = checked.prettyPrint.toString
}

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

    val result: mutable.Map[Fingerprint, Set[IdState]] = mutable.Map()

    val (uploaded, confirmed, revoked) = events.foldLeft(
      (
        Map[Fingerprint, Key](),
        Map[Identity, Fingerprint](),
        Map[Identity, Fingerprint]()
      )
    ) {
      case ((uploaded, confirmed, revoked), event) =>
        event match {
          case Event.Upload(key) =>
            (uploaded + (key.fingerprint -> key), confirmed, revoked)
          case Event.Revoke(ids, fingerprint) =>
            (
              uploaded,
              confirmed -- ids,
              revoked ++ (ids map (_ -> fingerprint))
            )
          case Event.Verify(ids, fingerprint) =>
            uploaded.get(fingerprint) match {
              case Some(key) if ids subsetOf key.identities =>
                (
                  uploaded,
                  confirmed ++ (ids map (_ -> fingerprint)),
                  revoked -- ids
                )
              case _ => (uploaded, confirmed, revoked)
            }
        }
    }

    val withUploaded = uploaded.foldLeft(result) { (acc, elem) =>
      acc + (elem._1 -> elem._2.identities.map((_, Private)))
    }

    val withUploadedAndConfirmed = confirmed
      .foldLeft(withUploaded) { (acc, elem) =>
        acc.get(elem._2) match {
          case Some(states) =>
            acc updated(elem._2, states - (elem._1 -> Private) - (elem._1 -> Revoked) + (elem._1 -> Public))
          case _ => acc
        }
      }
      .toMap

    val withUploadedAndConfirmedAndRevoked = revoked
      .foldLeft(withUploadedAndConfirmed) { (acc, elem) =>
        acc.get(elem._2) match {
          case Some(states) if states contains(elem._1, Public) =>
            acc updated(elem._2, states - (elem._1 -> Public) + (elem._1 -> Revoked))
          case _ => acc
        }
      }

    withUploadedAndConfirmedAndRevoked
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
   * Iterate over union of fingerprints returned by server and in history
   *
   * TODO: Generalize to accept both byFingerprint and byMail
   */
  def check[T](server: Spec1, which: T => Option[Key]): Map[Fingerprint, Map[Identity, EvalResult]] = {
    val responses = (identities.keys flatMap server.byFingerprint map (
      key => (key.fingerprint, key.identities)
      )).toMap

    val historyEval = identities
    val allKeys = historyEval.keys.toSet union responses.keys.toSet

    allKeys.foldLeft(Map.empty[Fingerprint, Map[Identity, EvalResult]]) {
      (acc, fingerprint) =>
        val historyIds = historyEval.getOrElse(fingerprint, Set.empty)
        val serverIds = responses.getOrElse(fingerprint, Set.empty)

        val checked = checkStates(serverIds, historyIds)

        acc + (fingerprint -> checked)
    }

  }

  private def checkStates(
                           identities: Set[Identity],
                           states: Set[(Identity, Status)]
                         ): Map[Identity, EvalResult] = {
    val allIds = identities union (states map (_._1))

    allIds.foldLeft(Map.empty[Identity, EvalResult]) { (acc, id) =>
      val serverState = identities.find(_ == id)
      val state = states.find(_._1 == id)

      val result: EvalResult = (state, serverState) match {
        case (None, None) => EvalResult.Ok
        case (None, Some(id)) => EvalResult.Mismatch(None, Some(id))

        case (Some((id, Private)), None) => EvalResult.Ok
        case (Some((id, Public)), None) =>
          EvalResult.Mismatch(Some((id, Public)), None)
        case (Some((id, Revoked)), None) => EvalResult.Ok

        case (Some((identity, Private)), Some(id)) =>
          EvalResult.Mismatch(Some((identity, Private)), Some(id))
        case (Some((identity, Public)), Some(id)) => EvalResult.Ok
        case (Some((identity, Revoked)), Some(id)) =>
          EvalResult.Mismatch(Some((identity, Revoked)), Some(id))
      }
      acc + (id -> result)
    }

  }

  override def toString: String = s"History(${events.size})"

  //  events
  //      .map(e => s"${e.getClass.getName} -> $e")
  //      .foldLeft("") { (acc: String, event: String) =>
  //        s"$acc\n$event "
  //      }

}

/**
 * Map(
 * FingerprintImpl(612d6529-43c7-49d5-869f-62724dc2bb07) ->
 * Map(
 * Identity(ilyaz@comcast.net) -> Mismatch(Some((Identity(ilyaz@comcast.net),Private)),Some(Identity(ilyaz@comcast.net))),
 * Identity(majordick@me.com) -> Ok),
 * FingerprintImpl(faf9794a-4429-499b-9c4b-4019f77531b9) ->
 * Map(
 * Identity(tezbo@att.net) -> Mismatch(Some((Identity(tezbo@att.net),Private)),Some(Identity(tezbo@att.net))),
 * Identity(rupak@mac.com) -> Ok))
 *
 *
 */

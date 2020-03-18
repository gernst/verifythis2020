package pgp

import scala.collection.mutable

/**
 * Client erweitern,
 * Liste von Entscheidungen -> So wie Messages : Alle Entscheidungen "high level"
 * Paare von Fingerprint und Identity
 * Global
 * 2 Arten von Nachrichten
 *  1. Entscheidung: Fingerprint verknüpft mit Menge an Identities
 *  2. Revoke von Verbindungen Fingerprint und Identity
 *
 * !!! History als Ablaufplan für Clients !!!
 *
 * History wird von ScalaCheck generiert.
 *
 * 2 Arten die History zu interpretieren:
 * -> Actor die die Events ausführen.
 * -> Checker, der nur die History verarbeitet.
 *
 * Auf Angreifer Seite,
 *
 * Checks über "History" checken, ob bestimmte Sichtbarkeiten okay sind.
 *
 * Angreifer vlt kein direkter Zugriff, stattdessen auch Messages in der Historie.
 * -> Prüfen, ob Historie so erlaubt ist.
 *
 * Egal über Fingerprint/KeyId/Email -> Immer Key filtern
 *
 * Funktionale Eigenschaften: Key ist selber bei Upload und download
 *
 *
 * Gegeben fingerprint -> Public identities auf basis der History zurückgeben.
 *
 *
 * Aufschreiben.
 *
 **/

sealed trait Event

/*
  Durch Histoy durchlaufen und sich Verify und Revoke anschauen.

*/

// das kann weg
case class Publish(fingerprint: Fingerprint) extends Event

case class Revoke(key: Key) extends Event

// vlt einfach Key als parameter
case class Assoc(key: Key) extends Event

case class Verify(ids: Set[Identity], fingerprint: Fingerprint) extends Event

case object Check extends Event


sealed trait Status

case object Public extends Status

case object Private extends Status

case object Revoked extends Status




trait State {
  type T

  def verified: Set[Identity]

  def associations: Map[Identity, Fingerprint]

  def update(event: Event): State = event match {
    case Assoc(ids, fingerprint) =>
      val ver = verified
      val assoc = associations
      new State {
        def verified: Set[Identity] = ver

        def associations: Map[Identity, Fingerprint] = ids.foldLeft(assoc) { (acc, v) => acc + (v -> fingerprint) }
      }

    case Verify(ids) => new State {
      def verified = ???

      def associations = ???
    }
    case Revoke(ids) => ???
    case Publish(_, _) => ???
  }
}

object HistoryEval {

  type IdState = (Identity, Status)


  /**
    * Returns a Map that associates a fingerprint with its identities that were added/verified/revoked during the
    * symbolic execution of the given history. Entries are marked with a Status that describes, whether the identity is
    * public/private or has been revoked
    */
  def identities(history: History): Map[Fingerprint, Set[IdState]] = {
    val acc = Map[Fingerprint, Set[IdState]]().withDefaultValue(Set.empty[IdState])


    history.history.foldLeft(acc) { (acc, event) => event match {
      case Assoc(key) => acc + ((key.fingerprint, key.identities.map(new IdState(_, Private))))


      case Revoke(key) => acc updated (key.fingerprint,
        acc(key.fingerprint) map { case (id, status) => if (key.identities contains id) (id, Revoked) else (id, status)}
        )
      case Verify(ids, fingerprint) => acc updated (fingerprint,
        acc(fingerprint) map { case (id, status) => if (ids contains id) (id, Public) else (id, status) })
      case _ =>
    }

    }
  }


  /**
    * Returns the set of identities and their status given the flow of [history]
    *
    */
  def publicIdentitiesFor(history: History, fingerprint: Fingerprint): Set[(Identity,Status)] = identities(history)(fingerprint) filter (_._2 == Public)


}


/**
 * Ohne State,
 * Überprüfen sollte separat bleiben
 */

class History {

  val history = mutable.Buffer[Event]()


  def todo(event: Event): State = event match {
    case Check => add(event); check(???, ???)
    case _ => add(event)
  }

  def add(event: Event): State = {
    val event = history.last
    val next = state update event

    history append ((event, next))

    next
  }

  def check(queryResult: Key, history: this.type): State = {

    /**
     * Step through history and collect knowledge about which identity is currently public/private etc.
     * At each point in the history, where the attacker inserted a Check-Event, we test, whether the given information
     * up to this point is sufficient to this point.
     */

    ???

  }

}

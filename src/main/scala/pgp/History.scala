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





object HistoryEval {

  type IdState = (Identity, Status)


  /**
    * Returns a Map that associates a fingerprint with its identities that were added/verified/revoked during the
    * symbolic execution of the given history. Entries are marked with a Status that describes, whether the identity is
    * public/private or has been revoked
    */
  def identities(history: History): Map[Fingerprint, Set[IdState]] = {
    val acc = Map[Fingerprint, Set[IdState]]().withDefaultValue(Set.empty[IdState])


    history.events.foldLeft(acc) { (acc, event) =>
      event match {
        case Assoc(key) => acc + ((key.fingerprint, key.identities.map(new IdState(_, Private))))
        case Revoke(key) => acc updated(key.fingerprint,
          acc(key.fingerprint) map { case (id, status) => if (key.identities contains id) (id, Revoked) else (id, status) }
        )
        case Verify(ids, fingerprint) => acc updated(fingerprint,
          acc(fingerprint) map { case (id, status) => if (ids contains id) (id, Public) else (id, status) })
        case _ => acc
      }
    }


  }


  /**
    * Returns the set of identities and their status given the flow of [history]
    *
    */
  def publicIdentitiesFor(history: History, fingerprint: Fingerprint): Set[(Identity,Status)] = identities(history)(fingerprint) filter (_._2 == Public)


}

case class History(events: mutable.Buffer[Event] = mutable.Buffer()) {

  def + (event: Event): Unit = { events append event }
}



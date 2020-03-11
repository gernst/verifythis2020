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
 *
 *
 * Auf Angreifer Seite,
 *
 * Checks über "History" checken, ob bestimmte Sichtbarkeiten okay sind.
 *
 * Angreifer vlt kein direkter Zugriff, stattdessen auch Messages in der Historie.
 * -> Prüfen, ob Historie so erlaubt ist.
 *
 *
 **/


sealed trait Event

case class Publish[T](given: T, fingerprint: Fingerprint) extends Event

case class Revoke(ids: Set[Identity]) extends Event

case class Assoc(ids: Set[Identity], fingerprint: Fingerprint) extends Event

case class Verify(ids: Set[Identity])

case object Check extends Event

sealed trait Status

case object Public extends Status

case object Private extends Status

case object Revoked extends Status

trait State {
  type T
  val verified: Set[Identity]
  val associations: Map[Identity, Fingerprint]

  def update(event: Event): State
}

object History {

  val history = mutable.Buffer[State]()

  def add(event: Event): State = {
    history append (history.last update event)

    history.last
  }

  def check() = {
    /**
     * Step through history and collect knowledge about which identity is currently public/private etc.
     * At each point in the history, where the attacker inserted a Check-Event, we test, whether the given information
     * up to this point is sufficient to this point.
     */
  }

}

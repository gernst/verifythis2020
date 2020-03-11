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

object History {

  val history = mutable.Buffer[(Event, State)]()

  def todo(event: Event): State = event match {
    case Check => add(event); check()
    case _ => add(event)
  }

  def add(event: Event): State = {
    val (_, state) = history.last
    val next = state update event

    history append ((event, next))

    next
  }

  def check(): State = {

    /**
     * Step through history and collect knowledge about which identity is currently public/private etc.
     * At each point in the history, where the attacker inserted a Check-Event, we test, whether the given information
     * up to this point is sufficient to this point.
     */

    ???

  }

}

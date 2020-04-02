package pgp

import scala.collection.mutable

trait Actor {
  def canAct: Boolean
  def act(): Unit

  val inbox = mutable.Queue[Data]()
  def canReceive = inbox.nonEmpty

  def isActive = canAct || canReceive

  def handle(from: Actor, msg: Message)
  def handle(from: Actor, msg: Body)
  def send(to: Actor, msg: Message): Unit = Network.send(this, to, msg)
  def send(to: Identity, msg: Body): Unit = Network.send(this, to, msg)

  def register(identity: Identity): Unit = Network.register(identity, this)
}

trait PassiveActor extends Actor {
  def canAct = false

  def act() = ???
}


// Mehrere Executions mit unterschiedlichen Strategies
object Execution {
  val actors = mutable.Buffer[Actor]()

  def step(rnd: Iterator[Int]): Boolean = {
    val enabled = actors filter (_.canReceive)

    if (enabled.nonEmpty) {
      val actor = choose(enabled, rnd)
      val data = actor.inbox.dequeue()
      data match {
        case Mail(from, to, msg) =>
          actor handle (from, msg)
        case Packet(from, to, msg) =>
          actor handle (from, msg)
      }
    }

    actors exists (_.isActive)
  }

  def sequence(server: Actor, actors: Actor*): Unit = {

    for (current <- actors) {
      current handle (server, Init)

      while (step(pgp.c(0))) {}
    }
  }
}

object Network {
  val mailboxes: mutable.Map[Identity, Actor] = mutable.Map[Identity, Actor]()

  def register(identity: Identity, actor: Actor): Unit = {
    mailboxes += (identity -> actor)
  }

  def send(from: Actor, to: Actor, msg: Message) {
    val pkt = Packet(from, to, msg)
    to.inbox enqueue pkt
  }

  def send(from: Actor, to: Identity, msg: Body): Unit = {
    val mail = Mail(from, to, msg)
    val actor = mailboxes(to)
    actor.inbox enqueue mail
  }
}
package pgp

import scala.collection.immutable.Queue

trait Actor {
  //  def state: ActorState
  //  // def run(in: Recv[In], out: Send[Out]): scala.concurrent.Future[Unit]
  //  def step(rnd: Iterator[Int]): Unit
  def handle(from: Actor, msg: Message)

  def handle(from: Actor, msg: Body)

  def send(to: Actor, msg: Message): Unit = Network.send(this, to, msg)

  def send(to: Identity, msg: Body): Unit = Network.send(this, to, msg)

  def register(identity: Identity): Unit = Network.register(identity, this)
}

object Network {
  var msgs: Queue[Data] = Queue()
  var mailHandler: Map[Identity, Actor] = Map()

  def register(identity: Identity, actor: Actor): Unit = mailHandler += (identity -> actor)

  def state: ActorState = Running

  def send(from: Actor, to: Actor, msg: Message) {
    val pkt = Packet(from, to, msg)
    msgs = msgs enqueue pkt
  }

  def send(from: Actor, to: Identity, msg: Body): Unit = {
    val mail = Mail(from, to, msg)
    msgs = msgs enqueue mail
  }

  // def run(in: Recv[In], out: Send[Out]): scala.concurrent.Future[Unit]
  def step(rnd: Iterator[Int]): Boolean = {
    if (msgs.nonEmpty) msgs.dequeue match {
      case (Mail(from, to, msg), queue) =>
        msgs = queue
        mailHandler.get(to) foreach (_ handle (from, msg))
      case (Packet(from, to, msg), queue) =>
        msgs = queue
        to handle (from, msg)
    }

    msgs.nonEmpty
  }

  def sequence(server: Actor, actors: Actor*): Unit = {

    for (current <- actors) {
      current handle (server, Init)

      while (step(pgp.c(0))) {}
    }
  }

  def handle(from: Actor, msg: Message): Unit = {}

  def handle(from: Actor, msg: Body): Unit = {}
}
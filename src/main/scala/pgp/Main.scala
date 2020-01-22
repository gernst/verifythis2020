package pgp

import pgp.backend.{Server, ServerActor}
import pgp.frontend.{NewClient, UploadNotValidatedKeySpec}
import pgp.types._

object Main extends App {

  val serverActor = new ServerActor(new Server)

  val identities = Identity.generate()

  val test = new UploadNotValidatedKeySpec(new NewClient(identities), serverActor)

  test.prepare()

  println(test.run())


}


//
//object OldMain {
//
//
//  def setupChannels() = {
//    Seq.fill(10) { (Channel.queue[ClientMessage], Channel.queue[ServerMessage]) }
//    // Channel.queues[ClientMessage](10) zip Channel.queues[ServerMessage](10)
//  }
//  /* Stream
//      .continually(Channel.queue[ClientMessage])
//      .take(10)
//      .zip(Stream.continually(Channel.queue[ServerMessage]).take(10))
//      .toSet */
//
//  def setupBackend() = new ServerActor(new Server)
//
//  def setupClients(
//                    channels: Seq[(Channel[ClientMessage], Channel[ServerMessage])]) =
//    channels
//      .map { case (send, recv) => new Client(recv.recv, send.send, Set()) }
//      .map(new ClientActor(_))
//
//  def run(
//           channels: Seq[(Channel[ClientMessage], Channel[ServerMessage])],
//           clients: Seq[ClientActor],
//           server: ServerActor): Unit = {
//    //TODO: Collect all futures and await for them collectively. NOT FINSIHED
//    val futures = channels.map {
//      case (clientChannel, serverChannel) =>
//        server.run(clientChannel.recv, serverChannel.send)
//    }.flatMap(???)
//    for ((Channel(_, in), Channel(out, _)) <- channels) {
//      // println("Hello World!")
//      server.run(in, out)
//    }
//    clients.foreach(_.run())
//  }
//
//  def execute(): Unit = {
//    val server = setupBackend()
//    val channels = setupChannels()
//
//    val clients = setupClients(channels)
//
//    run(channels, clients, server)
//
//  }
//
//  execute()
//
//
//  def step(actors: Seq[Actor[_, _]], rnd: Iterator[Int]): Unit = {
//    assert(actors.nonEmpty)
//    val active = choose(actors, rnd)
//    active.step(rnd)
//  }
//
//  def test() {
//    val actors: Seq[Actor[_, _]] = ???
//    step(actors, k(0, 1, 2))
//    // bzw server.step(c(0))
//  }
//
//  def main(): Unit = {
//    // init server
//
//    val addresses = Set(
//      "ilyaz@comcast.net",
//      "erynf@comcast.net",
//      "phish@verizon.net",
//      "empathy@yahoo.ca",
//      "peoplesr@optonline.net",
//      "crowl@verizon.net",
//      "ranasta@live.com",
//      "rupak@mac.com",
//      "wonderkid@yahoo.com",
//      "eminence@hotmail.com",
//      "crusader@sbcglobal.net",
//      "tezbo@att.net",
//      "mailarc@yahoo.com",
//      "majordick@me.com",
//      "jaffe@aol.com",
//      "mschilli@live.com",
//      "whimsy@yahoo.com",
//      "boser@yahoo.ca",
//      "bulletin@optonline.net",
//      "jonas@yahoo.ca",
//      "gator@hotmail.com",
//      "isotopian@outlook.com",
//      "formis@aol.com",
//      "hutton@outlook.com",
//      "fviegas@outlook.com",
//      "dkasak@msn.com",
//      "sopwith@live.com",
//      "horrocks@me.com",
//      "tfinniga@comcast.net",
//      "gfxguy@sbcglobal.net")
//
//    val identities = addresses map Identity
//
//    lazy val server = new Server
//
//    lazy val insecureServer = new ServerOld
//
//    val validateIdent: (Identity, EMail) => Unit = (identity, email) => {
//      log(s"> Received validating email. Verifying $identity", SECURE)
//      server.verify(email.token)
//    }
//
//    val validateIdentInsec: (Identity, EMail) => Unit = (identity, email) => {
//      log(s"> Received validating email. Verifying $identity", INSECURE)
//      insecureServer.verify(email.token)
//    }
//
//    log("Initializing both servers with test data...", BOTH)
//    log("Generating random keys...", BOTH)
//
//    val keys = (identities sliding (3, 3) map Key.random).toSet
//
//    log(s"Generated ${keys.size} keys with three identities each.", BOTH)
//    log("Uploading keys to servers...", BOTH)
//
//    val tokens = keys map (k => (k, server.upload(k)))
//    val tokensInsecure = keys map (k => (k, insecureServer.upload(k)))
//
//    log(
//      "Keys uploaded. Currently there should be 30 identities when querying by fingerprint/keyId",
//      BOTH)
//
//    def validatedByFingerprint(s: Spec1) =
//      keys
//        .flatMap(k => s.byFingerprint(k.fingerprint))
//        .flatMap(_.identities)
//
//    def validatedByEmail(s: Spec1) =
//      keys
//        .flatMap(_.identities)
//        .flatMap(s.byEmail)
//        .flatMap(_.identities)
//
//    log(
//      s"Validated (Fingerprint/KeyId): ${validatedByFingerprint(server).size}",
//      SECURE)
//    log(
//      s"Validated (Fingerprint/KeyId): ${validatedByFingerprint(insecureServer).size}",
//      INSECURE)
//
//    log(
//      "Currently, there should be 0 identities when querying a key by one of its associated identities",
//      BOTH)
//
//    log(s"Validated (Email): ${validatedByEmail(server).size}", SECURE)
//    log(
//      s"Validated (Email): ${validatedByEmail(insecureServer).size}",
//      INSECURE)
//
//    log("Verifying exactly one identity for 5 keys.", BOTH)
//    (
//      tokens
//        zip tokensInsecure
//      take (5)
//      foreach {
//        case (sec, insec) =>
//          val identSec = sec._1.identities.take(1)
//          val identInsec = insec._1.identities.take(1)
//          server.requestVerify(sec._2, identSec)
//          insecureServer.requestVerify(insec._2, identInsec)
//      })
//
//    log("There should be 5 validated identities", BOTH)
//
//    log(s"Validated (Email): ${validatedByEmail(server).size}", SECURE)
//    log(
//      s"Validated (Email): ${validatedByEmail(insecureServer).size}",
//      INSECURE)
//
//  }
//}

package pgp

import pgp.hagrid._

object Main extends App {

  val identities = Identity.mails map (Identity(_)) take 10

  val x = KeyGenerator.genPublicKey(identities.toSet)

  println(x)

}

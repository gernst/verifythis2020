package pgp

import pgp.hagrid._

object Main extends App {

  val id = "lukasrieger07@gmail.com"

  val identities = Identity(id)

  val x = KeyGenerator.genPublicKey(Set(identities))

  println(x)

}

package pgp


object Main extends App {

  //
  //  val id = "lukasrieger11@gmail.com"
  //
  //  val identities = Identity(id)
  //
  //
  //  println("Creating actual backend.")
  //
  //  val backend = new HagridServer
  //
  //  val k = Key.pgp(Set(identities))
  //
  //  val uploaded = backend.upload(k)
  //
  //  val getTheKey = backend.byFingerprint(k.fingerprint)
  //
  //  println("Trying to request verify...")
  //
  //  val verifyRequest = VerifyRequest(uploaded.uuid, List(identities.email)).asJson
  //
  //  println(verifyRequest)
  //
  //  val verifyResult = backend.requestVerify(uploaded, Set(identities))
  //
  //  println(uploaded)
  //  println(getTheKey)
  //  println(verifyResult)
  //
  //  println("verifying now.")
  //
  //  backend.verify(verifyResult.head.token)
  //
  //  val res = backend.byEmail(identities)
  //
  //  for {
  //    key <- res
  //  } println(key.keyId)

}

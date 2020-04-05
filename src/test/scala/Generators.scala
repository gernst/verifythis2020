import org.scalacheck.Gen
import pgp._

object Generators {

  val serverSecure = new ServerActor(new Server)
  val serverInsecure = new ServerActor(new ServerOld)
  val servers = List(serverSecure, serverInsecure)

  /**
   * TODO: Rename Spec1 to something more descriptive
   */
  val serverGen: Gen[Spec1] = Gen.oneOf(new Server, new Server)

  val serverActorGen: Gen[ServerActor] = Gen.oneOf(servers)

  def identityGen: Gen[Identity] =
    for (mail <- Gen.oneOf(Identity.mails)) yield Identity(mail)

  def identitySetGen(size: Int): Gen[Set[Identity]] =
    for (identities <- Gen.listOfN(size, identityGen)) yield identities.toSet

  def keyGen(idSize: Int): Gen[Key] =
    for (id <- identitySetGen(idSize)) yield Key.random(id)

}

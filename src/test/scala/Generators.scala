import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import pgp._

object Generators {

  val serverSecure = new ServerActor(new Server)
  val serverInsecure = new ServerActor(new ServerOld)
  val servers = List(serverSecure, serverInsecure)

  val serverActorGen: Gen[ServerActor] = Gen.oneOf(servers)


  val serverGen: Gen[Spec1] = for {
    rand <- arbitrary[Boolean]
    spec = if (rand) new Server else new ServerOld
  } yield spec

  def identityGen: Gen[Identity] =
    for (mail <- Gen.oneOf(Identity.mails)) yield Identity(mail)

  def identitySetGen(size: Int): Gen[Set[Identity]] =
    for (identities <- Gen.listOfN(size, identityGen)) yield identities.toSet

  implicit def keyGen(idSize: Int): Gen[Key] =
    for (id <- identitySetGen(idSize)) yield Key.random(id)

  implicit def keyGenActual(idSize: Int): Gen[Key] =
    for (id <- identitySetGen(idSize)) yield Key.pgp(id)

}

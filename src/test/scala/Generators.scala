import org.scalacheck.Gen
import pgp.{Identity, Key}

object Generators {

  def identityGen: Gen[Identity] = for (
    mail <- Gen.oneOf(Identity.mails)
  ) yield Identity(mail)

  def identitySetGen(size: Int): Gen[Set[Identity]] =
    Gen.listOfN(size, identityGen) flatMap (_.toSet)

  def keyGen(idSize: Int): Gen[Key] = for {
    id <- identitySetGen(idSize)
  } yield Key.random(id)

}

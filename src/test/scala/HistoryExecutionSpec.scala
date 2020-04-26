import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import pgp._

object HistoryExecutionSpec extends Properties("History execution") {

  import History.prettyPrint
  import HistoryGen.randomHistory

  val MinHistorySize = 20
  val MaxHistorySize = 50

  def sizedHistoryGen: Gen[Gen[History]] =
    for {
      size <- Gen.choose(MinHistorySize, MaxHistorySize)
    } yield randomHistory(size)

  implicit val arbitraryHistory: Arbitrary[Gen[History]] = Arbitrary(
    sizedHistoryGen
  )

  property("historyWithSecureServer") = forAll { gen: Gen[History] =>
    forAll(gen) { history =>
      val server = new Server
      Sequential.execute(server, history)
      val (fingerprintResult, mailResult) = history.check(server)

      val successFingerprint = mailResult.forall {
        case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
      }

      val successMail = fingerprintResult.forall {
        case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
      }

      if (!successFingerprint) println(prettyPrint(fingerprintResult))
      if (!successMail) println(prettyPrint(mailResult))

      successFingerprint && successMail

    }
  }

  property("historyWithInsecureServer") = forAll { gen: Gen[History] =>
    forAll(gen) { history =>
      val server = new ServerOld
      Sequential.execute(server, history)
      val (fingerprintResult, mailResult) = history.check(server)

      val successFingerprint = mailResult.forall {
        case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
      }

      val successMail = fingerprintResult.forall {
        case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
      }

      if (!successFingerprint) println(prettyPrint(fingerprintResult))
      if (!successMail) println(prettyPrint(mailResult))

      successFingerprint && successMail

    }
  }


}

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import pgp._

object HistoryExecutionSpec extends Properties("History execution") {

  import Generators.serverGen
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

  /**
   * Checking every history and the server after every execution spec
   */
  property("historyMatchesServerState") = forAll { gen: Gen[History] =>
    forAll(gen, serverGen) { (history, server) =>
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

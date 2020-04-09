import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import pgp._

object HistoryExecutionSpec extends Properties("History execution") {

  import Generators.serverGen
  import History.prettyPrint
  import HistoryGen.randomHistory

  val MinHistorySize = 20
  val MaxHistorySize = 50

  def anyServer(): Spec1 = new ServerOld()

  def sizedHistoryGen: Gen[Gen[History]] =
    for {
      size <- Gen.choose(MinHistorySize, MaxHistorySize)
    } yield randomHistory(size)

  implicit val arbitraryHistory: Arbitrary[Gen[History]] = Arbitrary(
    sizedHistoryGen
  )

  property("historyMatchesServerState") = forAll { gen: Gen[History] =>
    forAll(gen, serverGen) { (history, server) =>
      Sequential.execute(server, history)
      val result = history.check(server)

      val success = result.forall {
        case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
      }

      if (!success) println(prettyPrint(result))

      success

    }
  }

}

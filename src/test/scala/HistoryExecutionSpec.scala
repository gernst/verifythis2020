import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import pgp.{History, Sequential}

object HistoryExecutionSpec extends Properties("History execution") {

  val MinHistorySize = 10
  val MaxHistorySize = 50

  def sizedHistoryGen: Gen[Gen[History]] =
    for {
      size <- Gen.choose(MinHistorySize, MaxHistorySize)
    } yield HistoryGen.randomHistory(size)


  /**
   * Not sure about the nested forAlls.
   * Seems necessary to ensure that every history is executed on a fresh server instance.
   * Maybe this isn't necessary ??
   */
  property("historyMatchesServerState") = forAll(sizedHistoryGen) { gen =>
    forAll(gen, Generators.serverGen) { (history, server) =>
      val sequentialExec = Sequential.execute(server, history)
      val result = history.check(server)

      // the result of check is only valid if it returns an empty Map.
      if (result.nonEmpty) {
        println(result)
      }
      result.isEmpty
    }
  }
}

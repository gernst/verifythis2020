import java.nio.file.Paths

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import pgp.hagrid.HagridServer
import pgp.{EvalResult, _}

object HistoryExecutionSpec extends Properties("History execution") {

  import Generators.keyGenActual
  import HagridServer.remove
  import History.prettyPrint
  import HistoryGen.{minimalHistory, randomHistory}

  import sys.process._

  val MinHistorySize = 20
  val MaxHistorySize = 40

  val hagridPath = "/home/lukas/hagrid/hagrid"

  var hagridInstance = Process("./hagrid", new java.io.File(hagridPath)) run ProcessLogger(str => (), str => println(str))

  def resetHagrid(): Unit = {
    println("Destroying HAGRID state.")
    hagridInstance.destroy()
    remove(Paths.get(s"$hagridPath/state"))

    hagridInstance = Process("./hagrid", new java.io.File(hagridPath)) run ProcessLogger(str => (), str => println(str))

    println("Created new HAGRID backend.")
  }

  def sizedHistoryGen(implicit keyGen: Int => Gen[Key]): Gen[Gen[History]] =
    for {
      size <- Gen.choose(MinHistorySize, MaxHistorySize)
    } yield randomHistory(size)

  def minimalHistoryGen(implicit keyGen: Int => Gen[Key]): Gen[History] = minimalHistory(keyGen)


  implicit val arbitraryHistory: Arbitrary[Gen[History]] = Arbitrary(
    sizedHistoryGen(keyGenActual)
  )
  //
  //    property("historyWithSecureServer") = forAll { gen: Gen[History] =>
  //      forAll(gen) { history =>
  //        val server = new Server
  //        Sequential.execute(server, history)
  //        val (fingerprintResult, mailResult) = history.check(server)
  //
  //        val successFingerprint = mailResult.forall {
  //          case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
  //        }
  //
  //        val successMail = fingerprintResult.forall {
  //          case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
  //        }
  //
  //        if (!successFingerprint) println(prettyPrint(fingerprintResult))
  //        if (!successMail) println(prettyPrint(mailResult))
  //
  //        successFingerprint && successMail
  //
  //      }
  //    }
  //
  //    property("historyWithInsecureServer") = forAll { gen: Gen[History] =>
  //      forAll(gen) { history =>
  //        val server = new ServerOld
  //        Sequential.execute(server, history)
  //        val (fingerprintResult, mailResult) = history.check(server)
  //
  //        val successFingerprint = mailResult.forall {
  //          case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
  //        }
  //
  //        val successMail = fingerprintResult.forall {
  //          case (_, idMap) => idMap.forall(_._2 == EvalResult.Ok)
  //        }
  //
  //        if (!successFingerprint) println(prettyPrint(fingerprintResult))
  //        if (!successMail) println(prettyPrint(mailResult))
  //
  //        successFingerprint && successMail
  //
  //      }
  //    }

  property("historyWithHagridBackend") = forAll { gen: Gen[History] =>
    forAll(gen) { history =>
      val server = new HagridServer
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


  property("failingOnNonStaticHAGRID") = forAll(minimalHistoryGen) { history =>

    val server = new HagridServer
    Sequential.execute(server, history)

    resetHagrid()

    val (fingerprintResult, mailResult) = history.check(server)

    val successFingerprint = mailResult.forall {
      case (_, idMap) => idMap.forall(_._2 != EvalResult.Ok)
    }

    successFingerprint
  }

}

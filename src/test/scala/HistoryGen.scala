import org.scalacheck.Gen
import pgp.Event.{Upload, Verify}
import pgp.{Event, Fingerprint, History, Key}

import scala.collection.mutable

class Context(val keys: Map[Fingerprint, Key])

object HistoryGen {

  def minimalHistory(implicit keyGen: Int => Gen[Key]): Gen[History] =
    for {
      context <- contextGen(1, 1)(keyGen)
      key = context.keys.head._2
      events: mutable.Buffer[Event] = mutable.Buffer(
        Upload(context.keys.head._2),
        Verify(key.identities, key.fingerprint)
      )
    } yield History(events)


  def randomHistory(length: Int)(implicit keyGen: Int => Gen[Key]): Gen[History] =
    for {
      context <- contextGen(2, 2)(keyGen)
      events <- Gen.listOfN(length, eventGen(context))
    } yield History(events.toBuffer)

  /**
   * Generate a (reasonably sized context) that carries
   * PGPKeys, identities, etc. that will be used by the event generators.
   *
   * @param keySize   How many keys should be generated for this context
   * @param idsPerKey How many identities per key
   * @return Context
   */
  def contextGen(keySize: Int, idsPerKey: Int)(implicit keyGen: Int => Gen[Key]): Gen[Context] =
    for {
      keys <- Gen
        .listOfN(keySize, keyGen(keySize))
      keyMap = (keys map (k => (k.fingerprint, k))).toMap
    } yield new Context(keyMap)

  def eventGen(implicit context: Context): Gen[Event] =
    Gen.oneOf(uploadEventGen, verifyEventGen, revokeEventGen)

  def uploadEventGen(implicit context: Context): Gen[Event] =
    for ((_, key) <- Gen.oneOf(context.keys)) yield Event.Upload(key)

  def verifyEventGen(implicit context: Context): Gen[Event] =
    for {
      (fingerprint, key) <- Gen.oneOf(context.keys)
      identities <- Gen.someOf(key.identities)
    } yield Event.Verify(identities.toSet, fingerprint)

  def revokeEventGen(implicit context: Context): Gen[Event] =
    for {
      (fingerprint, key) <- Gen.oneOf(context.keys)
      identities <- Gen.someOf(key.identities)
    } yield Event.Revoke(identities.toSet, fingerprint)
}

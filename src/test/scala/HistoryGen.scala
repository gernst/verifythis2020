import org.scalacheck.Gen
import pgp.{Event, Fingerprint, History, Key}

import scala.collection.mutable

class Context(val keys: Map[Fingerprint, Key])

object HistoryGen {

  private def historyRec: Gen[mutable.Buffer[Event]] = ???

  /**
   * Priority: Low.
   * This should return a "random" history, in which the contained events are in a valid order
   * (e.g.: No Revoke/Verify before Upload)
   */
  def validHistory(length: Int): Gen[History] =
    for {
      events <- Gen.lzy(historyRec)
    } yield History(events)

  def randomHistory(length: Int): Gen[History] =
    for {
      context <- contextGen(2, 2)
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
  def contextGen(keySize: Int, idsPerKey: Int): Gen[Context] =
    for {
      keys <- Gen.listOfN(keySize, Generators.keyGen(idsPerKey))
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

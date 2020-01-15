package pgp

import pgp.types._
/**
 * Simple interface of a keyserver with validation
 */
trait Spec0 {
  def byEmail(identity: Identity): Option[Key]

  def requestAdd(identity: Identity, key: Key)
  def confirmAdd(token: Token)

  def requestDel(identity: Identity, key: Key)
  def confirmDel(token: Token)
}

object Spec0 {
  /**
   * Adapter that implements Spec0 with Spec1.
   */
  class from(backend: Spec1) extends Spec0 {
    def byEmail(identity: Identity): Option[Key] = {
      backend.byEmail(identity)
    }

    def requestAdd(identity: Identity, key: Key) {
      val token = backend.upload(key)
      backend.requestVerify(token, Set(identity))
    }

    def confirmAdd(token: Token) {
      backend.verify(token)
    }

    def requestDel(identity: Identity, key: Key) {
      backend.requestManage(identity)
    }

    def confirmDel(token: Token) = {
      /* Note: Can't implement this without additional information here.
       * 
       * The problem is that Spec1 gives management tokens
       * that are not specific to which identity shall be deleted.
       * We don't know the request for which this token was issued.
       */
      val identity: Identity = ???
      backend.revoke(token, Set(identity))
    }
  }
}

/**
 * Interface of the Hagrid keyserver
 */
trait Spec1 {
  // lookup via confirmed addresses
  def byEmail(identity: Identity): Option[Key]

  // lookup of unconfirmed data
  def byFingerprint(fingerprint: Fingerprint): Option[Key]
  def byKeyId(keyid: KeyId): Iterable[Key]

  // methods related key upload and identify verification
  def upload(key: Key): Token

  def requestVerify(from: Token, emails: Set[Identity]): Seq[EMail]
  def verify(token: Token)

  // methods to manage and remove identity associations
  def requestManage(identity: Identity): Option[EMail]
  def revoke(token: Token, emails: Set[Identity])
}

trait TestSpec {

  def prepare(): Unit

  def andThen(next: TestSpec, needsPrepare: Boolean): Boolean = {
    if (needsPrepare) next.prepare()
    run() & next.run()
  }

  def |>(next: TestSpec): Boolean = andThen(next, needsPrepare = false)

  def run(): Boolean
}


sealed trait ActorState

case object Running extends ActorState

case object Finished extends ActorState

package pgp

import scala.collection.mutable

object Sequential {

  /**
   * TODO: Rewrite this method to account for the fact that the given history might very well be nonsense.
   **/
  def execute(server: Spec1, history: History): Unit = {
    val uploaded: mutable.Map[Fingerprint, Token] = mutable.Map()
    for (event <- history.events) {
      event match {
        case Event.Upload(key) =>
          uploaded += ((key.fingerprint, server upload key))
        case Event.Revoke(identities, _) =>
          for (token <- server requestManage (identities.head) map (_.token)) {
            server revoke(token, identities)
          }
        case Event.Verify(identities, fingerprint) =>

          for {
            uploadToken <- uploaded.get(fingerprint)
            Body(_, token: Token, _) <- server requestVerify(uploadToken, identities)
          } server verify token
        //          val uploadToken = uploaded(fingerprint)
        //          for (Body(_, token: Token, _) <- server requestVerify(uploadToken, identities)) {
        //            server verify token
        //          }
        case Event.Check =>
      }
    }
  }

}

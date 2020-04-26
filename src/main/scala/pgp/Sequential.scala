package pgp

import scala.collection.mutable

object Sequential {

  def execute(server: Spec1, history: History): Unit = {
    val uploaded: mutable.Map[Fingerprint, Token] = mutable.Map()
    for (event <- history.events) {
      event match {
        case Event.Upload(key) =>
          uploaded += ((key.fingerprint, server upload key))
        case Event.Revoke(identities, _) =>
          for {
            head <- identities
            token <- server requestManage head map (_.token)
          } server revoke(token, Set(head))
        case Event.Verify(identities, fingerprint) =>
          for {
            uploadToken <- uploaded.get(fingerprint)
            Body(_, token, _) <- server requestVerify(uploadToken, identities)
          } server verify token
      }
    }
  }

}

package pgp

import scala.collection.mutable

object Sequential {

  def execute(server: Server, history: History): Unit = {
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
          val uploadToken = uploaded(fingerprint)
          for (Body(_, token: Token, _) <- server requestVerify(uploadToken, identities)) {
            server verify token
          }
        case Event.Check =>
      }
    }
  }

}

package pgp.hagrid

import pgp._
import sttp.client.{HttpURLConnectionBackend, NothingT, SttpBackend, basicRequest, Identity => SttpIdentity}
import sttp.model.Uri

object HagridServer {
  val baseURL = "127.0.0.1:8000"

  implicit val backend: SttpBackend[SttpIdentity, Nothing, NothingT] =
    HttpURLConnectionBackend()

  private def hagridRequest(location: String) =
    for {
      url <- Uri.parse(s"$baseURL$location").toOption
    } yield basicRequest.get(url)

  private def parseKeyOption(response: String): Option[Key] = ???

  private def parseKeyIterable(response: String): Iterable[Key] = ???
}

/**
 * For the sake of simplicity this remote server spec is synchronous and thus blocking.
 */
class HagridServer extends Spec1 {

  import HagridServer.{backend, hagridRequest, parseKeyIterable, parseKeyOption}

  override def byEmail(identity: Identity): Option[Key] =
    for {
      request <- hagridRequest("/vks/v1/by-email/<URI-ENCODED EMAIL-ADDRESS>")
      response <- request.send().body.toOption
      key <- parseKeyOption(response)
    } yield key

  override def byFingerprint(fingerprint: Fingerprint): Option[Key] =
    for {
      request <- hagridRequest(s"/vks/v1/by-fingerprint/${fingerprint.value}")
      response <- request.send().body.toOption
      key <- parseKeyOption(response)
    } yield key

  override def byKeyId(keyId: KeyId): Iterable[Key] = {
    val result = for {
      request <- hagridRequest(s"/vks/v1/by-fingerprint/${keyId.value}")
      response <- request.send().body.toOption
      key = parseKeyIterable(response)
    } yield key

    result.get
  }

  override def upload(key: Key): Token = {
    val result = for {
      request <- hagridRequest(s"/vks/v1/by-fingerprint/${key.value}")
      response <- request.send().body.toOption
      key = parseKey[Iterable](response)
    } yield key

    res
  }

  override def requestVerify(from: Token, emails: Set[Identity]): Seq[Body] =
    ???

  override def verify(token: Token): Unit = ???

  override def requestManage(identity: Identity): Option[EMail] = ???

  override def revoke(token: Token, emails: Set[Identity]): Unit = ???
}

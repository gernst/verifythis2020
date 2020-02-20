import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import pgp.ServerOld
import pgp.backend.{Server, ServerActor}
import pgp.frontend.{ByMailActor, NewClient, UploadActor, VerifyActor}
import pgp.types.{Actor, Identity, Key, Network}

object UploadSpec extends Properties("UploadSpec") {

  val serverSecure = new ServerActor(new Server)
  val serverInsecure = new ServerActor(new ServerOld)
  val servers = List(serverSecure, serverInsecure)

  def network(implicit network: Network): Network = network

  val serverGen: Gen[ServerActor] = Gen.oneOf(servers)

  val identityGen: Gen[Set[Identity]] = for {
    mail1 <- Gen.oneOf(Identity.mails())
    mail2 <- Gen.oneOf(Identity.mails())
  } yield Set(Identity(mail1), Identity(mail2))

  val clientGen: Gen[NewClient] = for {
    id <- identityGen
    key = Key.random(id)
  } yield NewClient(id, Set(key))

  val uploadSequenceGen: Gen[(NewClient, ServerActor, List[Actor])] = for {
    server <- serverGen
    client <- clientGen
  } yield
    (
      client,
      server,
      List(
        new UploadActor(client, client.keys.head._2),
        new VerifyActor(client),
        new ByMailActor(client)
      )
    )

  /**
    * If scalacheck finds an instance for which this property fails, it tries to reduce the parameters to
    * the "smallest" possible values.
    * In this case, this results in a uploadSequence that contains (client, server, () ) -> without any actors to run,
    * the constraint obviously can't hold.
    */
  property("upload") = forAll(uploadSequenceGen) {
    case (client, server, actors) =>
      network.sequence(server, actors: _*)

      client.received.forall { case (_, key) => key.identities.size == 1 }

  }
}

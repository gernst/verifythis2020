import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import pgp._

object UploadSpec extends Properties("UploadSpec") {

  val clientGen: Gen[Client] = for {
    id <- Generators.identitySetGen(2)
    key = Key.random(id)
  } yield Client(id, Set(key))

  val uploadSequenceGen: Gen[(Client, ServerActor, List[Actor])] = for {
    server <- Generators.serverActorGen
    client <- clientGen
  } yield
    (
      client,
      server,
      List(
        new UploadActor(client, client.keys.head._2, server),
        new VerifyActor(client, client.identities.head, server),
        new ByMailActor(client, client.identities.head, server)
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
      Execution.sequence(server, actors: _*)

      println(client.received.size)
      client.received.forall { case (_, key) => key.identities.size == 1 }

  }
}

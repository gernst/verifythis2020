import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object InformationFlow extends Properties("InformationFlow") {

  def secure(public: Int, secret: Int): Int = public

  def insecure(public: Int, secret: Int): Boolean = {
    public + secret >= 0
  }

  def test[A, B, C](public: A,
                    secret1: B,
                    secret2: B,
                    f: (A, B) => C): Boolean =
    f(public, secret1) == f(public, secret2)

  val smallIntGen: Gen[(Int, Int, Int)] = for {
    public <- Gen.choose(-100, 100)
    secret1 <- Gen.choose(-100, 100)
    secret2 <- Gen.choose(-100, 100)
  } yield (public, secret1, secret2)

  property("nonInterference") = forAll(smallIntGen) {
    case (public: Int, secret1: Int, secret2: Int) =>
      test(public, secret1, secret2, insecure)
  }

}

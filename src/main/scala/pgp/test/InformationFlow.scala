package pgp.test

import scala.util.Random

object InformationFlow {
  def secure(public: Int, secret: Int) = public

  def insecure(public: Int, secret: Int) = {
    public + secret >= 0
  }

  def test[A, B, C](public: A, secret1: B, secret2: B, f: (A, B) => C) {
    val f1 = f(public, secret1)
    val f2 = f(public, secret2)
    if(f1 != f2) {
      println()
      println("public:  " + public)
      println("secret1: " + secret1)
      println("secret2: " + secret2)
      println("result1: " + f1)
      println("result2: " + f2)
      assert(false)
    }
  }

  def main(args: Array[String]) {
    for (i <- 0 until 1000) {
      print(".")
      val public = Random.nextInt(10) - 5
      val secret1 = Random.nextInt(10) - 5
      val secret2 = Random.nextInt(10) - 5

      test(public, secret1, secret2, insecure)
    }
  }
}
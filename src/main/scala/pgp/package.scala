import scala.util.Random

/**
 * VerifyThis 2020 long term challenge
 * https://verifythis.github.io
 */
package object pgp {
  sealed trait LogType

  case object WARNING extends LogType
  case object SECURE extends LogType
  case object INSECURE extends LogType
  case object BOTH extends LogType

  def r(): Iterator[Int] = Iterator.continually(Random.nextInt)

  def k(is: Int*): Iterator[Int] = is.iterator

  def c(i: Int): Iterator[Int] = Iterator.continually(i)

  def choose[A](xs: Seq[A], rnd: Iterator[Int]): A = {
    xs(rnd.next().abs % xs.size)
  }

  def log(message: String, logType: LogType = SECURE): Unit = logType match {
    case SECURE => println(Console.GREEN + "[SECURE] " + message)
    case INSECURE => println(Console.YELLOW + "[INSECURE] " + message)
    case BOTH => println(Console.CYAN + "[INFO] " + message)
    case WARNING => println(Console.RED + "[WARNING] " + message)
  }
}


/**
 * VerifyThis 2020 long term challenge
 * https://verifythis.github.io
 */
package object pgp {
  import java.security.MessageDigest

  sealed trait LogType

    case object SECURE extends LogType
    case object INSECURE extends LogType
    case object BOTH extends LogType

    def log(message: String,logType: LogType = SECURE) = logType match {
        case SECURE => println(Console.YELLOW  + "[SECURE] "  +  message)
        case INSECURE => println(Console.RED  + "[INSECURE] "+ message)
        case BOTH => println(Console.CYAN + "[INFO] " + message)
    }
}
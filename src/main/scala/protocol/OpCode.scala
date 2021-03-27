package protocol

import scodec._
import scodec.bits._

sealed abstract class OpCode(val id: Byte, val name: String) extends Product with Serializable

object OpCode {
  case object OpError        extends OpCode(0x00, "ERROR")
  case object OpStartup      extends OpCode(0x01, "STARTUP")
  case object OpReady        extends OpCode(0x02, "READY")
  case object OpAuthenticate extends OpCode(0x03, "AUTHENTICATE")
  case object OpOptions      extends OpCode(0x05, "OPTIONS")
  case object OpSupported    extends OpCode(0x06, "SUPPORTED")

  val all: List[OpCode] =
    List(
      OpError,
      OpStartup,
      OpReady,
      OpAuthenticate,
      OpOptions,
      OpSupported
    )

  val byId: Byte => Option[OpCode] =
    all.map(x => x.id -> x).toMap.lift

  val codec = byte.exmap(
    b => Attempt.fromOption(byId(b), Err.General(s"Failed to find opcode for byte $b", Nil)),
    (b: OpCode) => Attempt.successful(b.id)
  )
}

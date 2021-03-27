package protocol

import scodec._
import scodec.bits._
import scodec.codecs._

sealed abstract class OpCode(val id:Byte, val name:String) extends Product with Serializable

object OpCode {
  case object OpError         extends OpCode(0x00, "ERROR")
  case object OpStartup       extends OpCode(0x01, "STARTUP")
  case object OpReady         extends OpCode(0x02, "READY")
  case object OpAuthenticate  extends OpCode(0x03, "AUTHENTICATE")
  // 0x04 intentionally missing
  case object OpOptions       extends OpCode(0x05, "OPTIONS")
  case object OpSupported     extends OpCode(0x06, "SUPPORTED")
  case object OpQuery         extends OpCode(0x07, "QUERY")
  case object OpResult        extends OpCode(0x08, "RESULT")
  case object OpPrepare       extends OpCode(0x09, "PREPARE")
  case object OpExecute       extends OpCode(0x0A, "EXECUTE")
  case object OpRegister      extends OpCode(0x0B, "REGISTER")
  case object OpEvent         extends OpCode(0x0C, "EVENT")
  case object OpBatch         extends OpCode(0x0D, "BATCH")
  case object OpAuthChallenge extends OpCode(0x0E, "AUTH_CHALLENGE")
  case object OpAuthResponse  extends OpCode(0x0F, "AUTH_RESPONSE")
  case object OpAuthSuccess   extends OpCode(0x10, "AUTH_SUCCESS")

  val all: List[OpCode] =
    List(OpError, OpStartup, OpReady, OpAuthenticate,
      OpOptions, OpSupported, OpQuery, OpResult,
      OpPrepare, OpExecute, OpRegister, OpEvent,
      OpBatch, OpAuthChallenge, OpAuthResponse,
      OpAuthSuccess)

  val byId: Byte => Option[OpCode] =
    all.map(x => x.id -> x).toMap.lift

  val codec = byte(8).exmap(b => Attempt.fromOption(byId(b), Err.General(s"Failed to find opcode for byte $b", Nil) ),(b:OpCode) => Attempt.successful(b.id))
}

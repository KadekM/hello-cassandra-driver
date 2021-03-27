package protocol.notations

// todo: throwaway?
import scodec.{Attempt, Codec, DecodeResult, Encoder, Err, SizeBound, codecs => s}

sealed abstract class OptionValue extends Product with Serializable
object OptionValue {
  case object Unknown extends OptionValue
}

sealed abstract class OptionPayload(val id:Int, val value:OptionValue) extends Product with Serializable
object OptionPayload {
  case object Unknown extends OptionPayload(0, OptionValue.Unknown)

  val all = List[OptionPayload](Unknown)

  val fromInt = all.map(x => x.id -> x).toMap.lift

  val codec = short.exmap(x => Attempt.fromOption(OptionPayload.fromInt(x), Err.General(s"Failed to find opcode for byte $x", Nil)), (x:OptionPayload) => Attempt.successful(x.id))
}

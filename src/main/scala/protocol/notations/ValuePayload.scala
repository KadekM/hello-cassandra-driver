package protocol.notations

import scodec.bits.BitVector
import scodec.{Attempt, Codec, Encoder, codecs => s}

sealed abstract class ValuePayload extends Product with Serializable
object ValuePayload {
  final case class ValueHolder(data:List[Int]) extends ValuePayload
  case object NullValue extends ValuePayload
  case object NotSetValue extends ValuePayload
  case object ErrValue extends ValuePayload

  val codec = {
    def nCodec(size:Int) = s.listOfN(s.provide(size), byte)
    val decoder = int.flatMap {
      case x if x == -1 => s.provide(ValuePayload.NullValue)
      case x if x == -2 => s.provide(ValuePayload.NotSetValue)
      case x if x < -2 => s.provide(ValuePayload.ErrValue)
      case x => nCodec(x).map(ValuePayload.ValueHolder)
    }
    val encoder = Encoder { x:ValuePayload =>
      x match {
        case ValuePayload.NullValue => Attempt.successful(BitVector.fromInt(-1))
        case ValuePayload.NotSetValue => Attempt.successful(BitVector.fromInt(-2))
        case ValuePayload.ErrValue => Attempt.successful(BitVector.fromInt(-3))
        case ValuePayload.ValueHolder(xs) => nCodec(xs.length).encode(xs)
      }
    }
    Codec(encoder, decoder)
  }
}

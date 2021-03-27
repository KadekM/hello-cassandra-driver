package protocol.notations

import scodec.bits.BitVector
import scodec.{Attempt, Codec, Encoder, codecs => s}

sealed abstract class BytesPayload extends Product with Serializable
object BytesPayload {
  final case class BytesHolder(data:List[Int]) extends BytesPayload
  case object NullBytes extends BytesPayload

  val codec:Codec[BytesPayload] = {
    def nCodec(size:Int) = s.listOfN(s.provide(size), byte)
    val decoder = int.flatMap {
      case x if x < 0 => s.provide(BytesPayload.NullBytes)
      case x => nCodec(x).map(BytesPayload.BytesHolder)
    }
    val encoder = Encoder { x:BytesPayload =>
      x match {
        case BytesPayload.NullBytes => Attempt.successful(BitVector.fromInt(-1))
        case BytesPayload.BytesHolder(xs) => nCodec(xs.length).encode(xs)
      }
    }
    Codec(encoder, decoder)
  }
}

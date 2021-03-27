package protocol

import scodec.bits.BitVector
import scodec.{Attempt, Encoder}

trait RequestEncoder[A] {
  def headerEncoder: Encoder[A]

  def bodyEncoder: Encoder[A]

  final def encode(a: A): Attempt[BitVector] = {
    for {
      header <- headerEncoder.encode(a)
      bodyVec <- bodyEncoder.encode(a)
      lenVec <- LengthPayload.codec.encode(LengthPayload(bodyVec.toByteVector.size.toInt)) // todo: toInt
    } yield header ++ lenVec ++ bodyVec
  }
}

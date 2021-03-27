package protocol

import scodec.bits.BitVector
import scodec.{Attempt, Encoder}

trait RequestEncoder[A <: FrameRequest] {
  def headerEncoder: Encoder[A] = Header.codec.contramap(_.header)

  def bodyEncoder: Encoder[A]

  final def encode(a: A): Attempt[BitVector] = {
    for {
      header  <- headerEncoder.encode(a)
      bodyVec <- bodyEncoder.encode(a)
      lenVec  <- LengthPayload.codec.encode(LengthPayload(bodyVec.toByteVector.size.toInt))
    } yield header ++ lenVec ++ bodyVec
  }
}

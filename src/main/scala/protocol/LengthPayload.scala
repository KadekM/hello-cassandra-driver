package protocol

import scodec.Codec
import scodec.codecs._

final case class LengthPayload(bodyLength:Int) extends AnyVal
object LengthPayload {
  val codec = int(4*8).xmap(LengthPayload.apply, (x:LengthPayload) => x.bodyLength)
  val empty = LengthPayload(0)
}

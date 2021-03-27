package protocol

import scodec.Codec
import scodec.codecs._
import scodec.bits.BitVector

/** MSB bit designates request/response, rest is protocol version */
final case class Version(direction: MessageDirection, protocolVersion: Byte)
sealed abstract class MessageDirection extends Product with Serializable
object MessageDirection {
  case object Request extends MessageDirection
  case object Response extends MessageDirection
}

object Version {
  val direction: Codec[MessageDirection] = bits(1).xmap({ b =>
    if (b.get(0)) MessageDirection.Response
    else MessageDirection.Request
  }, {
    case MessageDirection.Request => BitVector.zero
    case MessageDirection.Response => BitVector.one
  })

  val protocolVersion = bits(7)
    .xmap(_.toByte(signed = false), (x:Byte) => BitVector.fromByte(x, size = 7))

  val codec = (direction ~ protocolVersion)
    .xmap({
      Version.apply
    }, (x: Version) => (x.direction, x.protocolVersion))

}

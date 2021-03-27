package protocol

import scodec.Codec
import shapeless._

final case class Header(version: Version, flagsPayload: FlagsPayload, stream: StreamId, opCode: OpCode)

object Header {
  val codec: Codec[Header] = (Version.codec :: FlagsPayload.codec :: StreamId.codec :: OpCode.codec).xmap(
    { case v :: f :: sid :: op :: HNil =>
      Header(v, f, sid, op)
    },
    (x: Header) => x.version :: x.flagsPayload :: x.stream :: x.opCode :: HNil
  )
}

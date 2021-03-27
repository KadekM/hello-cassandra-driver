package protocol

import scodec.bits.BitVector
import scodec._

sealed trait Frame extends Product with Serializable {
  def header: Header
}

sealed trait FrameRequest extends Frame

sealed trait FrameResponse extends Frame

object FrameResponse {
  def decodeHeader(header: Header): Decoder[FrameResponse] = header.opCode match {
    case OpCode.OpSupported => stringMultiMap.map(x => SupportedResponse(header, x))
    case OpCode.OpReady     => codecs.provide(ReadyResponse(header))
    case h                  => Decoder(_ => Attempt.failure(Err(s"Unable to decode $h")))
  }
}

// requests

final case class OptionsRequest(protocolVersion: Byte, flagsPayload: FlagsPayload, streamId: StreamId)
    extends FrameRequest {
  def header: Header =
    Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpOptions)
}

object OptionsRequest {
  implicit val optionsRequestEncoder: RequestEncoder[OptionsRequest] = new RequestEncoder[OptionsRequest] {
    def bodyEncoder: Encoder[OptionsRequest] = Encoder[OptionsRequest] { _: OptionsRequest =>
      Attempt.successful(BitVector.empty)
    }
  }
}

final case class StartupRequest(protocolVersion: Byte, flagsPayload: FlagsPayload, streamId: StreamId, body: StringMap)
    extends FrameRequest {
  def header: Header =
    Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpStartup)
}

object StartupRequest {
  implicit val startupRequestEncoder: RequestEncoder[StartupRequest] = new RequestEncoder[StartupRequest] {
    def bodyEncoder: Encoder[StartupRequest] = stringMap.contramap((x: StartupRequest) => x.body)
  }
}

// responses

final case class ReadyResponse(header: Header) extends FrameResponse

final case class SupportedResponse(header: Header, body: StringMultiMap) extends FrameResponse

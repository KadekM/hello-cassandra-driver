package protocol.frame

import protocol._
import protocol.notations._
import scodec.{Attempt, Codec, Decoder, Encoder, Err, codecs => s}

sealed trait Frame extends Product with Serializable {
  def header:Header
}

object Frame {
}

sealed trait FrameRequest extends Frame
sealed trait FrameResponse extends Frame

object FrameResponse {
  def decodeHeader(header:Header):Decoder[FrameResponse] = header.opCode match {
    case OpCode.OpError => ErrorResponse.bodyCodec.map { case (err, txt) => frame.ErrorResponse(header, err, txt) }
    case OpCode.OpReady => s.provide(ReadyResponse(header))
    case OpCode.OpAuthenticate => AuthenticateResponse.bodyCodec.map { authenticator => AuthenticateResponse(header, authenticator) }
    case OpCode.OpSupported => SupportedResponse.bodyCodec.map(x => SupportedResponse(header, x))
    case OpCode.OpResult => ResultResponse.bodyDecoder.map(x => ResultResponse(header, x))
    case OpCode.OpEvent => EventResponse.bodyDecoder.map(x => EventResponse(header, x))
    case OpCode.OpAuthChallenge => AuthChallengeResponse.bodyCodec.map(x => AuthChallengeResponse(header, x))
    case OpCode.OpAuthSuccess => AuthSuccessResponse.bodyCodec.map(x => AuthSuccessResponse(header, x))
    case h => Decoder(_ => Attempt.failure(Err(s"Unable to decode $h")))
  }
}

// requests

final case class StartupRequest(protocolVersion:Byte, flagsPayload:FlagsPayload, streamId:StreamId, body:StringMap) extends FrameRequest {
  def header:Header = Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpStartup)
}

object StartupRequest {
  implicit val startupRequestEncoder:RequestEncoder[StartupRequest] = new RequestEncoder[StartupRequest] {
    def headerEncoder: Encoder[StartupRequest] = Header.codec.contramap(_.header)
    def bodyEncoder: Encoder[StartupRequest] = stringMap.contramap((x:StartupRequest) => x.body)
  }
}

final case class AuthResponseRequest(protocolVersion:Byte, flagsPayload:FlagsPayload, streamId:StreamId, token:BytesPayload) extends FrameRequest {
  def header:Header = Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpAuthResponse)
}

object AuthResponseRequest {
  implicit val authResponseRequestEncoder:RequestEncoder[AuthResponseRequest] = new RequestEncoder[AuthResponseRequest] {
    def headerEncoder: Encoder[AuthResponseRequest] = ???
    def bodyEncoder: Encoder[AuthResponseRequest] = ???
  }
}

final case class OptionsRequest(protocolVersion:Byte, flagsPayload:FlagsPayload, streamId:StreamId) extends FrameRequest {
  def header:Header = Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpOptions)
}

object OptionsRequest {
  implicit val optionsRequestEncoder:RequestEncoder[OptionsRequest] = new RequestEncoder[OptionsRequest] {
    def headerEncoder: Encoder[OptionsRequest] = Header.codec.contramap(_.header)
    def bodyEncoder: Encoder[OptionsRequest] = emptyEncoder[OptionsRequest]
  }
}

// todo
final case class QueryRequest(protocolVersion:Byte, flagsPayload:FlagsPayload, streamId:StreamId,
                              query:String, consistency:Consistency, todo:Any) extends FrameRequest {
  def header:Header = Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpQuery)
}

object QueryRequest {
  implicit val authResponseRequestEncoder:RequestEncoder[QueryRequest] = new RequestEncoder[QueryRequest] {
    def headerEncoder: Encoder[QueryRequest] = ???
    def bodyEncoder: Encoder[QueryRequest] = ???
  }
}

final case class PrepareRequest(protocolVersion:Byte, flagsPayload:FlagsPayload, streamId:StreamId) extends FrameRequest {
  def header:Header = Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpPrepare)
}

object PrepareRequest {
  implicit val authResponseRequestEncoder:RequestEncoder[PrepareRequest] = new RequestEncoder[PrepareRequest] {
    def headerEncoder: Encoder[PrepareRequest] = ???
    def bodyEncoder: Encoder[PrepareRequest] = ???
  }
}

final case class ExecuteRequest(protocolVersion:Byte, flagsPayload:FlagsPayload, streamId:StreamId) extends FrameRequest {
  def header:Header = Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpExecute)
}

object ExecuteRequest {
  implicit val authResponseRequestEncoder:RequestEncoder[ExecuteRequest] = new RequestEncoder[ExecuteRequest] {
    def headerEncoder: Encoder[ExecuteRequest] = ???
    def bodyEncoder: Encoder[ExecuteRequest] = ???
  }
}

final case class BatchRequest(protocolVersion:Byte, flagsPayload:FlagsPayload, streamId:StreamId) extends FrameRequest {
  def header:Header = Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpBatch)
}

object BatchRequest {
  implicit val authResponseRequestEncoder:RequestEncoder[BatchRequest] = new RequestEncoder[BatchRequest] {
    def headerEncoder: Encoder[BatchRequest] = ???
    def bodyEncoder: Encoder[BatchRequest] = ???
  }
}

final case class RegisterRequest(protocolVersion:Byte, flagsPayload:FlagsPayload, streamId:StreamId) extends FrameRequest {
  def header:Header = Header(Version(MessageDirection.Request, protocolVersion), flagsPayload, streamId, OpCode.OpRegister)
}

object RegisterRequest {
  implicit val authResponseRequestEncoder:RequestEncoder[RegisterRequest] = new RequestEncoder[RegisterRequest] {
    def headerEncoder: Encoder[RegisterRequest] = ???
    def bodyEncoder: Encoder[RegisterRequest] = ???
  }
}

// responses
final case class ErrorResponse(header:Header, errCode:Int, text:String) extends FrameResponse
object ErrorResponse {
  val bodyCodec:Codec[(UShortRepre, String)] = int ~ string
}

final case class ReadyResponse(header:Header) extends FrameResponse
object ReadyResponse {
}

final case class AuthenticateResponse(header:Header, authenticator:String) extends FrameResponse
object AuthenticateResponse {
  val bodyCodec:Codec[String] = string
}

final case class SupportedResponse(header:Header, body:StringMultiMap) extends FrameResponse
object SupportedResponse {
  val bodyCodec:Codec[Map[String, List[String]]] = stringMultiMap
}


final case class ResultResponse(header:Header, result:ResultBody) extends FrameResponse
object ResultResponse {
  val bodyDecoder:Decoder[ResultBody] = ResultBody.decoder
}

final case class EventResponse(header:Header, event:Event) extends FrameResponse
object EventResponse {
  val bodyDecoder:Decoder[Event] = Event.decoder
}
final case class AuthChallengeResponse(header:Header, challengeToken:BytesPayload) extends FrameResponse
object AuthChallengeResponse {
  val bodyCodec:Codec[BytesPayload] = BytesPayload.codec
}

final case class AuthSuccessResponse(header:Header, finalToken:BytesPayload) extends FrameResponse
object AuthSuccessResponse {
  val bodyCodec:Codec[BytesPayload] = BytesPayload.codec
}


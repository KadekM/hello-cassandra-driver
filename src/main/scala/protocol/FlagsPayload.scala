package protocol

import protocol._
import protocol.FlagsPayload._
import scodec.Codec
import scodec.codecs._

final case class FlagsPayload(private[protocol] val flags:Byte) {
  def flag(tpe:FlagType):Boolean = flags.flag(tpe.byte)
  def flagsMap:Map[FlagType, Boolean] = FlagsPayload.flags.map(x => x -> flag(x)).toMap
}

object FlagsPayload {
  sealed abstract class FlagType(val byte:Byte) extends Product with Serializable
  case object CompressionFlag     extends FlagType(0x01)
  case object TracingFlag         extends FlagType(0x02)
  case object CustomPayloadFlag   extends FlagType(0x04)
  case object WarningFlag         extends FlagType(0x08)
  case object UseBetaFlag         extends FlagType(0x10)

  val codec = byte(8).xmap(FlagsPayload.apply, (x:FlagsPayload) => x.flags)
  val flags = Set(CompressionFlag, TracingFlag, CustomPayloadFlag, WarningFlag, UseBetaFlag)

  def apply(flags:Set[FlagType]):FlagsPayload = {
    val b = flags.foldLeft(0x00) { _ | _.byte }.toByte
    FlagsPayload(b)
  }

  def apply(flags:FlagType*):FlagsPayload = apply(flags.toSet)
}


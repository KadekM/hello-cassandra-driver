package protocol.frame

import scodec.{Attempt, Codec, DecodeResult, Encoder, Err, SizeBound, codecs => s}

sealed abstract class TypeOption(val id:Int) extends Product with Serializable
object TypeOption {
  /** the skips in ids are intentional */
  case object TypeCustom    extends TypeOption(0x0000)
  case object TypeAscii     extends TypeOption(0x0001)
  case object TypeBigint    extends TypeOption(0x0002)
  case object TypeBlob      extends TypeOption(0x0003)
  case object TypeBoolean   extends TypeOption(0x0004)

  case object TypeCounter   extends TypeOption(0x0005)
  case object TypeDecimal   extends TypeOption(0x0006)
  case object TypeDouble    extends TypeOption(0x0007)
  case object TypeFloat     extends TypeOption(0x0008)
  case object TypeInt       extends TypeOption(0x0009)

  case object TypeTimestamp extends TypeOption(0x000B)
  case object TypeUuid      extends TypeOption(0x000C)
  case object TypeVarchar   extends TypeOption(0x000D)
  case object TypeVarInt    extends TypeOption(0x000E)
  case object TypeTimeUuid  extends TypeOption(0x000F)
  case object TypeInet      extends TypeOption(0x0010)

  case object TypeDate      extends TypeOption(0x0011)
  case object TypeTime      extends TypeOption(0x0012)
  case object TypeSmallInt  extends TypeOption(0x0013)
  case object TypeTinyInt   extends TypeOption(0x0014)
  case object TypeDuration  extends TypeOption(0x0015)

  case object TypeList      extends TypeOption(0x0020)
  case object TypeMap       extends TypeOption(0x0021)
  case object TypeSet       extends TypeOption(0x0022)
  case object TypeUdt       extends TypeOption(0x0030)
  case object TypeTuple     extends TypeOption(0x0031)

  val all = List[TypeOption](
    TypeCustom, TypeAscii, TypeBigint, TypeBlob, TypeBoolean,
    TypeCounter, TypeDecimal, TypeDouble, TypeFloat, TypeInt,
    TypeTimestamp, TypeUuid, TypeVarchar, TypeVarInt, TypeInet,
    TypeDate, TypeTime, TypeSmallInt, TypeTinyInt, TypeDuration,
    TypeList, TypeMap, TypeSet, TypeUdt, TypeTuple)

  val fromInt = all.map(x => x.id -> x).toMap.lift

  val codec = ???
}



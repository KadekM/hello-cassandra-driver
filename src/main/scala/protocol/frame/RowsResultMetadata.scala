package protocol.frame

import protocol._
import protocol.notations._
import scodec.{Codec, Decoder, Err, codecs => s}
import protocol.notations.BytesPayload

final case class RowsResultMetadata(flags:RowsResultMetadataFlags, columnsCount:Int, pagingState:Option[BytesPayload], newMetadataId:List[Int],
                                    globalTableSpec:Option[TableSpec], colSpecs:List[Any])

final case class RowsResultMetadataFlags(private[protocol] val flags:Int)

object RowsResultMetadataFlags {
  sealed abstract class MetadataFlagType(val id:Int) extends Product with Serializable {
    def flag(tpe:MetadataFlagType):Boolean = id.flag(tpe.id)
    def flagsMap:Map[MetadataFlagType, Boolean] = RowsResultMetadataFlags.flags.map(x => x -> flag(x)).toMap
  }
  case object GlobalTablesSpec  extends MetadataFlagType(0x0001)
  case object HasMorePages      extends MetadataFlagType(0x0002)
  case object NoMetadata        extends MetadataFlagType(0x0004)
  case object MetadataChanged   extends MetadataFlagType(0x0008)

  val codec = int.xmap(RowsResultMetadataFlags.apply, (x:RowsResultMetadataFlags) => x.flags)
  val flags = Set(GlobalTablesSpec, HasMorePages, NoMetadata, MetadataChanged)

  def apply(flags:Set[MetadataFlagType]):RowsResultMetadataFlags = {
    val b = flags.foldLeft(0x0000) { _ | _.id }
    RowsResultMetadataFlags(b)
  }

  def apply(flags:MetadataFlagType*):RowsResultMetadataFlags = apply(flags.toSet)
}


package protocol.frame

import protocol.notations._
import scodec.{Codec, Decoder, Err, codecs => s}

sealed trait ResultBody extends Product with Serializable
object ResultBody {
  val decoder:Decoder[ResultBody] = int.flatMap {
    case 0x01 => s.provide(VoidResultBody)
    case 0x02 => RowsResultBody.codec
    case 0x03 => SetKeySpaceResultBody.codec
    case 0x04 => s.provide(PreparedResultBody())
    case 0x05 => SchemaChangeResultBody.codec
    case x => s.fail(Err(s"Unable to decode result of $x"))
  }
}

case object VoidResultBody extends ResultBody
final case class RowsResultBody(metadata:RowsResultMetadata, rowsCount:Int, rowsContent:List[Row]) extends ResultBody
object RowsResultBody {
  //todo
  val codec:Decoder[RowsResultBody] = ???
}

final case class SetKeySpaceResultBody(keyspaceName:String) extends ResultBody
object SetKeySpaceResultBody {
  val codec:Codec[SetKeySpaceResultBody] = string.xmap(SetKeySpaceResultBody.apply, (x:SetKeySpaceResultBody) => x.keyspaceName)
}

final case class PreparedResultBody() extends ResultBody
object PreparedResultBody {
}

final case class SchemaChangeResultBody(schemaChange:SchemaChange) extends ResultBody
object SchemaChangeResultBody {
  val codec:Codec[SchemaChangeResultBody] = SchemaChange.codec.xmap(SchemaChangeResultBody.apply, (x:SchemaChangeResultBody) => x.schemaChange)
}


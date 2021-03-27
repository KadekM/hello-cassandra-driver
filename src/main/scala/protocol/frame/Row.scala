package protocol.frame

import protocol.notations.BytesPayload
import scodec.{Codec, codecs => s}

final case class Row(values:List[BytesPayload])
object Row {
  def codec(columnsCount:Int):Codec[Row] = s.listOfN(s.provide(columnsCount), BytesPayload.codec).xmap(Row.apply, (x:Row) => x.values)
}

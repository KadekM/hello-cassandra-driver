package protocol.frame

import scodec.{Codec, codecs => s}

final case class RowsContent(rows:List[Row])
object RowsContent {
  def codec(rowsCount:Int, columnsCount:Int):Codec[RowsContent] =
    s.listOfN(s.provide(rowsCount), Row.codec(columnsCount)).xmap(RowsContent.apply, (x:RowsContent) => x.rows)
}

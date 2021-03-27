package protocol.frame

import protocol.notations._
import scodec.Codec

final case class TableSpec(ksName:String, tableName:String)

object TableSpec {
  val codec:Codec[TableSpec] = (string ~ string).xmap(x => TableSpec(x._1, x._2), (x:TableSpec) => (x.ksName, x.tableName))
}

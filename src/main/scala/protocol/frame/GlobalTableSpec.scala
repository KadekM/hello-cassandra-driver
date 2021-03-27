package protocol.frame

import scodec.Codec

final case class GlobalTableSpec(spec:TableSpec) extends AnyVal

object GlobalTableSpec {
  val codec:Codec[GlobalTableSpec] = TableSpec.codec.xmap(GlobalTableSpec.apply, (x:GlobalTableSpec) => x.spec)
}

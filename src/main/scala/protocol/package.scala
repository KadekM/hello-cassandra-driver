import scodec._

package object protocol {
  /** from spec, section 3 */
  val int: Codec[Int]   = codecs.int(4 * 8)
  val long: Codec[Long] = codecs.long(8 * 8)
  val byte: Codec[Byte] = codecs.byte(1 * 8)
  val short: Codec[Int]       = codecs.uint(2 * 8)
  val string: Codec[String]           = codecs.variableSizeBytes(short, codecs.utf8)
  val longString: Codec[String]       = codecs.variableSizeBytes(int, codecs.utf8)
  val stringList: Codec[List[String]] = codecs.listOfN(short, string)
  val shortBytes: Codec[List[Byte]]    = codecs.listOfN(short, byte)

  type StringMap      = Map[String, String]
  type StringMultiMap = Map[String, List[String]]

  val stringMap: Codec[StringMap] = codecs.listOfN(short, string ~ string).xmap(_.toMap, _.toList)
  val stringMultiMap: Codec[StringMultiMap] =
    codecs.listOfN(short, string ~ stringList).xmap(_.toMap, _.toList)
}

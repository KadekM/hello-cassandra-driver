package protocol

import java.util.UUID

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Encoder, Err, SizeBound, codecs => s}
import scala.util.Try

/** from spec, section 3 */
package object notations {
  val int:Codec[Int] = s.int(4 * 8)
  val long:Codec[Long] = s.long(8 * 8)
  type UByteRepre = Int
  val byte:Codec[UByteRepre] = s.uint(1 * 8)
  type UShortRepre = Int
  val short:Codec[UShortRepre] = s.uint(2 * 8)
  val string:Codec[String] = s.variableSizeBytes(short, s.utf8)
  val longString:Codec[String] = s.variableSizeBytes(int, s.utf8)
  val uuid:Codec[CassandraUuid] = CassandraUuid.codec
  val stringList:Codec[List[String]] = s.listOfN(short, string)
  val bytes:Codec[BytesPayload] = BytesPayload.codec
  val value:Codec[ValuePayload] = ValuePayload.codec
  val shortBytes:Codec[List[Int]] = s.listOfN(short, byte)
  val options:Codec[OptionPayload] = OptionPayload.codec
  val optionList:Codec[List[OptionPayload]] = s.listOfN(short, options)
  val inetAddr:Codec[InetAddr] = InetAddr.codec
  val inet:Codec[Inet] = Inet.codec
  val consistency:Codec[Consistency] = Consistency.codec
  type StringMap = Map[String, String]
  val stringMap:Codec[Map[String, String]] = s.listOfN(short, string ~ string).xmap(_.toMap, _.toList)
  type StringMultiMap = Map[String, List[String]]
  val stringMultiMap:Codec[Map[String, List[String]]] = s.listOfN(short, string ~ stringList).xmap(_.toMap, _.toList)
  type BytesMap = Map[String, BytesPayload]
  val bytesMap:Codec[Map[String, BytesPayload]] = s.listOfN(short, string ~ bytes).xmap(_.toMap, _.toList)
}

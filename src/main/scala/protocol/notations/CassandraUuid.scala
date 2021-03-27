package protocol.notations

import scodec.{Attempt, Codec, DecodeResult, Encoder, Err, SizeBound, codecs => s}

/** Cassandra UUID is 16bytes long */
final case class CassandraUuid(payload:Long) extends AnyVal

object CassandraUuid {
  val codec = s.long(64).xmap(CassandraUuid.apply, (x:CassandraUuid) => x.payload)
}

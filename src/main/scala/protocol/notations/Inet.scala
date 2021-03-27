package protocol.notations

import protocol.notations.InetType._
import scodec.{Attempt, Codec, DecodeResult, Encoder, Err, SizeBound, codecs => s}

sealed abstract class InetType(val addrSize:UByteRepre) extends Product with Serializable
object InetType {
  case object Ipv4Type extends InetType(4)
  case object Ipv16Type extends InetType(16)

  val all = List(Ipv4Type, Ipv16Type)

  val byAddr: UByteRepre => Option[InetType] = all.map(x => x.addrSize -> x).toMap.lift

  val codec:Codec[InetType] = byte.exmap(x => Attempt.fromOption(byAddr(x), Err.General(s"$x Inet type not found", Nil)), (x:InetType) => Attempt.successful(x.addrSize))
}

final case class InetAddr(tpe:InetType, addresses:List[UByteRepre])
object InetAddr {
  val codec = {
    def addr(tpe:InetType) = s.listOfN(s.provide(tpe.addrSize), byte)

    val decoder = for {
      inetTpe <- InetType.codec
      d <- addr(inetTpe)
    } yield InetAddr(inetTpe, d)

    val encoder = Encoder { x: InetAddr =>
      for {
        inetTpe <- InetType.codec.encode(x.tpe)
        data <- addr(x.tpe).encode(x.addresses)
      } yield inetTpe ++ data
    }

    Codec(encoder, decoder)
  }
}

final case class Inet(addr:InetAddr, port:Int)
object Inet {
  val codec = (InetAddr.codec ~ int).xmap(x => Inet(x._1, x._2), (x:Inet) => (x.addr, x.port))
}

import protocol._
import scodec.Attempt
import scodec.bits.BitVector
import zio._
import zio.nio.channels._
import zio.nio.core._

trait BitVectorSocket {
  def write(vector: BitVector): Task[Unit]

  def read(numBytes: Int): Task[BitVector]
}

object BitVectorSocket {
  def apply(host: String, port: Int): TaskManaged[BitVectorSocket] = {
    for {
      inet   <- ZManaged.fromEffect(SocketAddress.inetSocketAddress(host, port))
      socket <- SocketChannel.open(inet)
    } yield new BitVectorSocket {
      override def write(vector: BitVector): Task[Unit] = {
        val buffer = Buffer.byteFromJava(vector.toByteBuffer)
        socket.write(buffer).unit
      }

      override def read(numBytes: Int): Task[BitVector] = {
        socket.readChunk(numBytes).map(x => BitVector.bits(x.asBits))
      }
    }
  }
}

trait MessageSocket {
  def send[A <: FrameRequest: RequestEncoder](req: A): Task[Unit]

  def receive: Task[FrameResponse]
}

object MessageSocket {
  private def fromAttempt[T](attemt: Attempt[T]): Task[T] =
    attemt.fold(x => ZIO.fail(new Exception(x.messageWithContext)), x => ZIO.succeed(x))

  def apply(host: String, port: Int): TaskManaged[MessageSocket] = {
    for {
      socket <- BitVectorSocket(host, port)
    } yield new MessageSocket {
      override def send[A <: FrameRequest](req: A)(implicit encoder: RequestEncoder[A]): Task[Unit] = {
        val encoded = fromAttempt(encoder.encode(req))
        encoded.flatMap(socket.write)
      }

      override def receive: Task[FrameResponse] = {

        val bytes       = (Header.codec.sizeBound.lowerBound / 8).toInt
        val lengthBytes = (LengthPayload.codec.sizeBound.lowerBound / 8).toInt
        for {
          headerBits     <- socket.read(bytes)
          header         <- fromAttempt(Header.codec.decodeValue(headerBits))
          bodyLengthBits <- socket.read(lengthBytes)
          bodyLength     <- fromAttempt(LengthPayload.codec.decodeValue(bodyLengthBits))
          bodyBits       <- socket.read(bodyLength.bodyLength)
          bodyDecoder    = FrameResponse.decodeHeader(header)
          body           <- fromAttempt(bodyDecoder.decodeValue(bodyBits))
        } yield body
      }
    }

  }
}

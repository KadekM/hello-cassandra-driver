
import protocol.LengthPayload
import scodec._
import scodec.bits.BitVector

package object protocol {
  def emptyEncoder[A]:Encoder[A] = Encoder[A]{ _:A => Attempt.successful(BitVector.empty)}

  implicit final class ByteUtils(private val b: Byte) extends AnyVal {
    def msb: Boolean = b != 0

    def lsb: Boolean = (b & 1) == 1

    def zeroMsb: Byte = ((b << 1) >> 1).toByte

    def flag(a: Byte): Boolean = (b & a) != 0
  }

  implicit final class IntUtils(private val b: Int) extends AnyVal {
    def flag(a: Int): Boolean = (b & a) != 0
  }

}



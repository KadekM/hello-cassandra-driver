
import scodec.bits.BitVector
import zio._

object HelloBitVector extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val zio = BitVectorSocket("localhost", 9042).use { bvs =>
      bvs.write(BitVector.bit(true))
    }

    zio.exitCode
  }
}

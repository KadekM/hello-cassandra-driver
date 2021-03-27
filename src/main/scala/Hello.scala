
import protocol.FlagsPayload._
import protocol._
import protocol.frame._
import scodec.bits.BitVector
import zio._
import zio.logging.Logging

object Hello extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val flags = FlagsPayload(TracingFlag, WarningFlag)
    val version = Version(MessageDirection.Request, 4)

    val request1 = OptionsRequest(version.protocolVersion, flags, StreamId(1))
    val request2 = StartupRequest(version.protocolVersion, flags, StreamId(2), Map("CQL_VERSION" -> "3.0.0"))

    val zio = for {
      _ <- ZIO.unit
      bvsM = MessageSocket("localhost", 9042)
      _ <- bvsM.use { bvs =>
        for {
          _ <- bvs.send(request1)
          resp1 <- bvs.receive
          _ <- logging.log.info(resp1.toString)
          _ <- bvs.send(request2)
          resp2 <- bvs.receive
          _ <- logging.log.info(resp2.toString)
        } yield ()
      }
    } yield ()

    val loggingLayer = Logging.console()
    zio.provideCustomLayer(loggingLayer).exitCode
  }
}

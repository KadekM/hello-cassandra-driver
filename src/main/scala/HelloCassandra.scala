import protocol.FlagsPayload._
import protocol._
import zio._
import zio.logging._

object HelloCassandra extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val flags   = FlagsPayload(TracingFlag, WarningFlag)
    val version = Version(MessageDirection.Request, 4)

    val request1 = OptionsRequest(version.protocolVersion, flags, StreamId(1))
    val request2 = StartupRequest(version.protocolVersion, flags, StreamId(2), Map("CQL_VERSION" -> "3.0.0"))

    val zio = for {
      _ <- MessageSocket("localhost", 9042).use { socket =>
        for {
          _     <- log.info(s"request: $request1")
          _     <- socket.send(request1)
          resp1 <- socket.receive
          _     <- log.info(s"response: ${resp1.toString}")

          _     <- log.info(s"request: $request2")
          _     <- socket.send(request2)
          resp2 <- socket.receive
          _     <- log.info(s"response: ${resp2.toString}")
        } yield ()
      }
    } yield ()

    val loggingLayer = Logging.console()
    zio.provideCustomLayer(loggingLayer).exitCode
  }
}

package protocol

final case class LengthPayload(bodyLength: Int) extends AnyVal
object LengthPayload {
  val codec = int.xmap(LengthPayload.apply, (x: LengthPayload) => x.bodyLength)
}

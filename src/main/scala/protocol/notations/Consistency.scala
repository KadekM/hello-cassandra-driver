package protocol.notations

import scodec.{Attempt, Err}


sealed abstract class Consistency(val id:UShortRepre, val name:String) extends Product with Serializable

object Consistency {
  case object Any         extends Consistency(0x0000, "ANY")
  case object One         extends Consistency(0x0001, "ONE")
  case object Two         extends Consistency(0x0002, "TWO")
  case object Three       extends Consistency(0x0003, "THREE")
  case object Quorum      extends Consistency(0x0004, "QUORUM")
  case object All         extends Consistency(0x0005, "ALL")
  case object LocalQuorum extends Consistency(0x0006, "LOCAL_QUORUM")
  case object EachQuorum  extends Consistency(0x0007, "EACH_QUORUM")
  case object Serial      extends Consistency(0x0008, "SERIAL")
  case object LocalSerial extends Consistency(0x0009, "LOCAL_SERIAL")
  case object LocalOne    extends Consistency(0x000A, "LOCAL_ONE")

  val all: List[Consistency] =
    List(Any, One, Two, Three,
      Quorum, All, LocalQuorum, EachQuorum,
      Serial, LocalSerial, LocalOne)

  val byId: UShortRepre => Option[Consistency] =
    all.map(x => x.id -> x).toMap.lift

  val codec = short.exmap(b => Attempt.fromOption(byId(b), Err.General(s"Failed to find consistency for $b", Nil) ),(b:Consistency) => Attempt.successful(b.id))
}

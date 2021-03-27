package protocol.frame

import protocol._
import protocol.frame.StatusChangeEvent.StatusChangeType
import protocol.frame.TopologyChangeEvent.TopologyChangeType
import protocol.notations._
import scodec.{Attempt, Codec, Decoder, Encoder, Err, codecs => s}

sealed trait Event extends Product with Serializable
object Event {
  val decoder:Decoder[Event] = string.flatMap {
    case "TOPOLOGY_CHANGE" => TopologyChangeEvent.codec
    case "STATUS_CHANGE" => StatusChangeEvent.codec
    case "SCHEMA_CHANGE" => SchemaChangeEvent.codec
    case x => s.fail(Err(s"Unable to decode event of $x"))
  }
}

final case class TopologyChangeEvent(tpe:TopologyChangeType, inet:InetAddr) extends Event
object TopologyChangeEvent {
  val codec:Codec[TopologyChangeEvent] = (TopologyChangeType.codec ~ InetAddr.codec).xmap(x => TopologyChangeEvent(x._1, x._2), (x:TopologyChangeEvent) => (x.tpe, x.inet))

  sealed abstract class TopologyChangeType(val id:String) extends Product with Serializable
  object TopologyChangeType {
    case object NewNodeInTopology extends TopologyChangeType("NEW_NODE")
    case object RemovedNodeInTopology extends TopologyChangeType("REMOVED_NODE")
    val all = List(NewNodeInTopology, RemovedNodeInTopology)
    val byId: String => Option[TopologyChangeType] = all.map(x => x.id -> x).toMap.lift
    val codec:Codec[TopologyChangeType] =
      string.exmap(x => Attempt.fromOption(byId(x), Err.General(s"Topology change type $x not found", Nil)), (x:TopologyChangeType) => Attempt.successful(x.id))
  }
}

final case class StatusChangeEvent(tpe:StatusChangeType, inet:InetAddr) extends Event
object StatusChangeEvent {
  val codec:Codec[StatusChangeEvent] = (StatusChangeType.codec ~ InetAddr.codec).xmap(x => StatusChangeEvent(x._1, x._2), (x:StatusChangeEvent) => (x.tpe, x.inet))

  sealed abstract class StatusChangeType(val id:String) extends Product with Serializable
  object StatusChangeType {
    case object StatusUp extends StatusChangeType("UP")
    case object StatusDown extends StatusChangeType("DOWN")
    val all = List(StatusUp, StatusDown)
    val byId: String => Option[StatusChangeType] = all.map(x => x.id -> x).toMap.lift
    val codec:Codec[StatusChangeType] =
      string.exmap(x => Attempt.fromOption(byId(x), Err.General(s"Status change type $x not found", Nil)), (x:StatusChangeType) => Attempt.successful(x.id))
  }
}

final case class SchemaChangeEvent(change:SchemaChange) extends Event
object SchemaChangeEvent {
  val codec:Codec[SchemaChangeEvent] = SchemaChange.codec.xmap(SchemaChangeEvent.apply, (x:SchemaChangeEvent) => x.change)
}


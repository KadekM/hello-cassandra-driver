package protocol.frame

import protocol.frame.SchemaChange._
import protocol.notations._
import scodec.{Attempt, Codec, Decoder, Encoder, Err, codecs => s}
import shapeless._

final case class SchemaChange(tpe:SchemaChangeType, change:SchemaChangeTargetOptions)
object SchemaChange {
  val codec:Codec[SchemaChange] = (SchemaChangeType.codec :: SchemaChangeTargetOptions.codec)
    .xmap({ case tpe :: change :: HNil => SchemaChange(tpe, change)}, (x:SchemaChange) => x.tpe :: x.change :: HNil)

  sealed abstract class SchemaChangeType(val id:String) extends Product with Serializable
  object SchemaChangeType {
    case object SchemaCreated extends SchemaChangeType("CREATED")
    case object SchemaUpdated extends SchemaChangeType("UPDATED")
    case object SchemaDropped extends SchemaChangeType("DROPPED")

    val all = List(SchemaCreated, SchemaUpdated, SchemaDropped)
    val byId: String => Option[SchemaChangeType] = all.map(x => x.id -> x).toMap.lift
    val codec:Codec[SchemaChangeType] =
      string.exmap(x => Attempt.fromOption(byId(x), Err.General(s"Schema change type $x not found", Nil)), (x:SchemaChangeType) => Attempt.successful(x.id))
  }

  // a bit meh
  sealed trait SchemaChangeTargetOptions extends Product with Serializable
  object SchemaChangeTargetOptions {
    val codec:Codec[SchemaChangeTargetOptions] = {
      val decoder:Decoder[SchemaChangeTargetOptions] = string.flatMap {
        case "KEYSPACE" => SchemaKeyspaceChange.codec
        case "TABLE" => SchemaTableChange.codec
        case "TYPE" => SchemaTypeChange.codec
        case "FUNCTION" => SchemaFunctionChange.codec
        case "AGGREGATE" => SchemaAggregateChange.codec
        case x => s.fail(Err(s"Unable to decode result of $x"))
      }

      val encoder:Encoder[SchemaChangeTargetOptions] = Encoder { x: SchemaChangeTargetOptions =>
        x match {
          case inst:SchemaKeyspaceChange => for (u <- string.encode("KEYSPACE"); v <- SchemaKeyspaceChange.codec.encode(inst)) yield u ++ v
          case inst:SchemaTableChange => for (u <- string.encode("TABLE"); v <- SchemaTableChange.codec.encode(inst)) yield u ++ v
          case inst:SchemaTypeChange => for (u <- string.encode("TYPE"); v <- SchemaTypeChange.codec.encode(inst)) yield u ++ v
          case inst:SchemaFunctionChange => for (u <- string.encode("FUNCTION"); v <- SchemaFunctionChange.codec.encode(inst)) yield u ++ v
          case inst:SchemaAggregateChange => for (u <- string.encode("AGGREGATE"); v <- SchemaAggregateChange.codec.encode(inst)) yield u ++ v
        }
      }
      Codec(encoder, decoder)
    }
  }
  final case class SchemaKeyspaceChange(keyspace:String) extends SchemaChangeTargetOptions
  object SchemaKeyspaceChange {
    val codec:Codec[SchemaKeyspaceChange] = string.xmap(SchemaKeyspaceChange.apply, (x:SchemaKeyspaceChange) => x.keyspace)
  }

  final case class SchemaTableChange(keyspace:String, name:String) extends SchemaChangeTargetOptions
  object SchemaTableChange {
    val codec:Codec[SchemaTableChange] = (string ~ string).xmap(x => SchemaTableChange(x._1, x._2), (x:SchemaTableChange) => (x.keyspace, x.name))
  }

  final case class SchemaTypeChange(keyspace:String, name:String) extends SchemaChangeTargetOptions
  object SchemaTypeChange {
    val codec:Codec[SchemaTypeChange] = (string ~ string).xmap(x => SchemaTypeChange(x._1, x._2), (x:SchemaTypeChange) => (x.keyspace, x.name))
  }

  final case class SchemaFunctionChange(keyspace:String, name:String, args:List[String]) extends SchemaChangeTargetOptions
  object SchemaFunctionChange {
    val codec:Codec[SchemaFunctionChange] = (string :: string :: stringList)
      .xmap( {case key :: name :: args :: HNil => SchemaFunctionChange(key, name, args) }, (x:SchemaFunctionChange) => x.keyspace :: x.name :: x.args :: HNil)
  }

  final case class SchemaAggregateChange(keyspace:String, name:String, args:List[String]) extends SchemaChangeTargetOptions
  object SchemaAggregateChange {
    val codec:Codec[SchemaAggregateChange] = (string :: string :: stringList)
      .xmap( {case key :: name :: args :: HNil => SchemaAggregateChange(key, name, args) }, (x:SchemaAggregateChange) => x.keyspace :: x.name :: x.args :: HNil)
  }
}


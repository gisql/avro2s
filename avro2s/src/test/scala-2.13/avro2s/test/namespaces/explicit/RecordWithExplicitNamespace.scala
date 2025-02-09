/** GENERATED CODE */

package avro2s.test.namespaces.explicit

import org.apache.avro.AvroRuntimeException

import scala.annotation.switch
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

case class RecordWithExplicitNamespace(var _string: String, var _record_with_namespace_inherited_from_direct_parent: avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedFromDirectParent, var _array_of_records: List[avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedViaArray], var _map_of_records: Map[String, avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedViaMap], var _union_of_records: avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedViaUnion :+: String :+: CNil) extends org.apache.avro.specific.SpecificRecordBase {
  def this() = this(null, null, null, null, null)
  
  override def getSchema: org.apache.avro.Schema = RecordWithExplicitNamespace.SCHEMA$
  
  override def get(field$: Int): AnyRef = {
    (field$: @switch) match {
      case 0 => _string.asInstanceOf[AnyRef]
      case 1 => _record_with_namespace_inherited_from_direct_parent.asInstanceOf[AnyRef]
      case 2 => _array_of_records match {
        case array =>
          scala.jdk.CollectionConverters.BufferHasAsJava({
            array.map { x =>
              x.asInstanceOf[AnyRef]
            }
          }.toBuffer).asJava
        }
      case 3 => {
        val map: java.util.HashMap[String, Any] = new java.util.HashMap[String, Any]
        _map_of_records.foreach { kvp =>
          val key = kvp._1
          val value = {
            kvp._2
          }
          map.put(key, value)
        }
        map
      }.asInstanceOf[AnyRef]
      case 4 => _union_of_records match {
        case Inl(x) => x.asInstanceOf[AnyRef]
        case Inr(Inl(x)) => x.asInstanceOf[AnyRef]
        case _ => throw new AvroRuntimeException("Invalid value")
      }
      case _ => new org.apache.avro.AvroRuntimeException("Bad index")
    }
  }
  
  override def put(field$: Int, value: Any): Unit = {
    (field$: @switch) match {
      case 0 => this._string = value.toString.asInstanceOf[String]
      case 1 => this._record_with_namespace_inherited_from_direct_parent = value.asInstanceOf[avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedFromDirectParent]
      case 2 => this._array_of_records = {
        value match {
          case array: java.util.List[_] =>
            scala.jdk.CollectionConverters.IteratorHasAsScala(array.iterator).asScala.map({ value =>
              value.asInstanceOf[avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedViaArray]
            }).toList
          }
      }
      case 3 => this._map_of_records = {
        value match {
          case map: java.util.Map[_,_] => {
            scala.jdk.CollectionConverters.MapHasAsScala(map).asScala.toMap map { kvp =>
              val key = kvp._1.toString
              val value = kvp._2
              (key, {
                value.asInstanceOf[avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedViaMap]
              })
            }
          }
        }
      }
      case 4 => value match {
        case x: avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedViaUnion => this._union_of_records = Coproduct[avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedViaUnion :+: String :+: CNil](x)
        case x: org.apache.avro.util.Utf8 => this._union_of_records = Coproduct[avro2s.test.namespaces.explicit.RecordWithNamespaceInheritedViaUnion :+: String :+: CNil](x.toString)
        case _ => throw new AvroRuntimeException("Invalid value")
      }
    }
  }
}

object RecordWithExplicitNamespace {
  val SCHEMA$: org.apache.avro.Schema = new org.apache.avro.Schema.Parser().parse("""{"type":"record","name":"RecordWithExplicitNamespace","namespace":"avro2s.test.namespaces.explicit","fields":[{"name":"_string","type":"string"},{"name":"_record_with_namespace_inherited_from_direct_parent","type":{"type":"record","name":"RecordWithNamespaceInheritedFromDirectParent","fields":[{"name":"_string","type":"string"},{"name":"_record_with_namespace_inherited_from_indirect_non_top_level_parent","type":{"type":"record","name":"RecordWithNamespaceInheritedFromIndirectNonTopLevelParent","fields":[{"name":"_string","type":"string"}]}}]}},{"name":"_array_of_records","type":{"type":"array","items":{"type":"record","name":"RecordWithNamespaceInheritedViaArray","fields":[{"name":"_string","type":"string"}]}}},{"name":"_map_of_records","type":{"type":"map","values":{"type":"record","name":"RecordWithNamespaceInheritedViaMap","fields":[{"name":"_string","type":"string"}]}}},{"name":"_union_of_records","type":[{"type":"record","name":"RecordWithNamespaceInheritedViaUnion","fields":[{"name":"_string","type":"string"}]},"string"]}]}""")
}
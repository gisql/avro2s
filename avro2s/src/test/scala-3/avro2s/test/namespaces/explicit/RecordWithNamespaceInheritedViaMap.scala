/** GENERATED CODE */

package avro2s.test.namespaces.explicit

import scala.annotation.switch

case class RecordWithNamespaceInheritedViaMap(var _string: String) extends org.apache.avro.specific.SpecificRecordBase {
  def this() = this(null)
  
  override def getSchema: org.apache.avro.Schema = RecordWithNamespaceInheritedViaMap.SCHEMA$
  
  override def get(field$: Int): AnyRef = {
    (field$: @switch) match {
      case 0 => _string.asInstanceOf[AnyRef]
      case _ => new org.apache.avro.AvroRuntimeException("Bad index")
    }
  }
  
  override def put(field$: Int, value: Any): Unit = {
    (field$: @switch) match {
      case 0 => this._string = value.toString.asInstanceOf[String]
    }
  }
}

object RecordWithNamespaceInheritedViaMap {
  val SCHEMA$: org.apache.avro.Schema = new org.apache.avro.Schema.Parser().parse("""{"type":"record","name":"RecordWithNamespaceInheritedViaMap","namespace":"avro2s.test.namespaces.explicit","fields":[{"name":"_string","type":"string"}]}""")
}
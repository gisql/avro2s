/** GENERATED CODE */

package avro2s.test.unions

import scala.annotation.switch

case class Record1(var field1: String) extends org.apache.avro.specific.SpecificRecordBase {
  def this() = this(null)
  
  override def getSchema: org.apache.avro.Schema = Record1.SCHEMA$
  
  override def get(field$: Int): AnyRef = {
    (field$: @switch) match {
      case 0 => field1.asInstanceOf[AnyRef]
      case _ => new org.apache.avro.AvroRuntimeException("Bad index")
    }
  }
  
  override def put(field$: Int, value: Any): Unit = {
    (field$: @switch) match {
      case 0 => this.field1 = value.toString.asInstanceOf[String]
    }
  }
}

object Record1 {
  val SCHEMA$: org.apache.avro.Schema = new org.apache.avro.Schema.Parser().parse("""{"type":"record","name":"Record1","namespace":"avro2s.test.unions","fields":[{"name":"field1","type":"string"}]}""")
}
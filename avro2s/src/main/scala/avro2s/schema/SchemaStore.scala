package avro2s.schema

import org.apache.avro.Schema

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

class SchemaStore {
  val schemas: scala.collection.concurrent.Map[String, Schema] = {
    new ConcurrentHashMap[String, Schema]().asScala
  }

  def accept(schema: Schema): Unit = {
    val fullName = schema.getFullName
    val _ = schemas += (fullName -> schema)
  }
}

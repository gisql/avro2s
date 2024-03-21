package avro2s.generator.specific.scala2.record

import avro2s.generator
import avro2s.generator.FunctionalPrinter
import avro2s.generator.specific.scala2.FieldOps._
import avro2s.generator.specific.scala2.record.TypeHelpers.UnionRepresentation.{CoproductRepresentation, OptionRepresentation}
import avro2s.generator.specific.scala2.record.TypeHelpers._
import org.apache.avro.Schema
import org.apache.avro.Schema.Type
import org.apache.avro.Schema.Type._

/**
 * NOTE: This features code that is not stack safe, based on the expectation that deeply nested schemas are unlikely, and that build tools
 * can adjust the stack size, if needed, when running code generation, without impacting applications. This may be improved in the future.
 */
private[avro2s] object PutCaseGenerator {
  def printFieldCase(printer: FunctionalPrinter, index: Int, field: Schema.Field): FunctionalPrinter = {
    field.schema().getType match {
      case UNION =>
        printer
          .add(s"case $index => value match {")
          .indent
          .call(printUnionPatternMatch(_, field.name, unionSchemasToType(schemas(field.schema()))))
          .outdent
          .add("}")
      case BYTES =>
        printer
          .add(s"case $index => this.${field.safeName} = value match {")
          .indent
          .add("case buffer: java.nio.ByteBuffer =>")
          .indent
          .add("val array = Array.ofDim[Byte](buffer.remaining())")
          .add("buffer.get(array)")
          .add("array")
          .outdent
          .add("}")
          .outdent
      case MAP =>
        printMapCase(printer, index, field)
      case ARRAY =>
        printArrayCase(printer, index, field)
      case _ =>
        printer
          .add(s"case $index => this.${field.safeName} = ${toStringConverter("value", field.schema())}.asInstanceOf[${schemaToScalaType(field.schema)}]")
    }
  }

  private def printArrayCase(printer: FunctionalPrinter, index: Int, field: Schema.Field): FunctionalPrinter =
    printer
      .add(s"case $index => this.${field.safeName} = {")
      .indent
      .call(printArrayValueOuter(_, field.schema()))
      .outdent
      .add("}")

  private def printArrayValueOuter(printer: FunctionalPrinter, schema: Schema, valueName: Option[String] = None): FunctionalPrinter = {
    val value = valueName.getOrElse("value")
    printer
      .add(s"$value match {")
      .indent
      .add("case array: java.util.List[_] =>")
      .indent
      .add("scala.jdk.CollectionConverters.IteratorHasAsScala(array.iterator).asScala.map({ value =>")
      .indent
      .call(printArrayValueInner(_, schema.getElementType))
      .outdent
      .add("}).toList")
      .outdent
      .add("}")
      .outdent
  }

  private def printArrayValueInner(printer: FunctionalPrinter, schema: Schema, valueName: Option[String] = None): FunctionalPrinter = {
    val value = valueName.getOrElse("value")
    schema.getType match {
      case UNION =>
        printer
          .add(s"$value match {")
          .indent
          .call(printUnionValue(_, unionSchemasToType(schemas(schema))))
          .outdent
          .add("}")
      case MAP => printMapValueOuter(printer, schema)
      case ARRAY => printArrayValueOuter(printer, schema, Some(value))
      case BYTES =>
        printer
          .add(s"$value match {")
          .indent
          .add("case buffer: java.nio.ByteBuffer =>")
          .indent
          .add("val array = Array.ofDim[Byte](buffer.remaining())")
          .add("buffer.get(array)")
          .add("array")
          .outdent
          .add("}")
          .outdent
      case _ =>
        printer
          .add(typeCast(value, schema))
    }
  }

  private def printMapCase(printer: FunctionalPrinter, index: Int, field: Schema.Field): FunctionalPrinter =
    printer
      .add(s"case $index => this.${field.safeName} = {")
      .indent
      .call(printMapValueOuter(_, field.schema()))
      .outdent
      .add("}")

  private def printMapValueOuter(printer: FunctionalPrinter, schema: Schema): FunctionalPrinter =
    printer
      .add("value match {")
      .indent
      .add("case map: java.util.Map[_,_] => {")
      .indent
      .add("scala.jdk.CollectionConverters.MapHasAsScala(map).asScala.toMap map { kvp =>")
      .indent
      .add("val key = kvp._1.toString")
      .add("val value = kvp._2")
      .add(s"(key, {")
      .indent
      .call(printMapValueInner(_, schema.getValueType))
      .outdent
      .add("})")
      .outdent
      .add("}")
      .outdent
      .add("}")
      .outdent
      .add("}")

  private def printMapValueInner(printer: FunctionalPrinter, schema: Schema): FunctionalPrinter = {
    schema.getType match {
      case UNION =>
        printer
          .add("value match {")
          .indent
          .call(printUnionValue(_, unionSchemasToType(schemas(schema))))
          .outdent
          .add("}")
      case MAP => printMapValueOuter(printer, schema)
      case ARRAY =>
        printer
          .add("value match {")
          .indent
          .add("case array: java.util.List[_] =>")
          .indent
          .add("scala.jdk.CollectionConverters.IteratorHasAsScala(array.iterator).asScala.map({ value =>")
          .indent
          .call(printArrayValueInner(_, schema.getElementType))
          .outdent
          .add("}).toList")
          .outdent
          .add("}")
          .outdent
      case BYTES =>
        printer
          .add("value match {")
          .indent
          .add("case buffer: java.nio.ByteBuffer =>")
          .indent
          .add("val array = Array.ofDim[Byte](buffer.remaining())")
          .add("buffer.get(array)")
          .add("array")
          .outdent
          .add("}")
          .outdent
      case _ =>
        printer
          .add(typeCast("value", schema))
    }
  }

  private def printUnionPatternMatch(printer: FunctionalPrinter, fieldName: String, union: UnionRepresentation): FunctionalPrinter = {
    union match {
      case cr: CoproductRepresentation => printer.add({
        cr.types.map { t =>
          t.getType match {
            case MAP =>
              indentedFp()(_.call(printUnionMapPatternMatch(_, fieldName, t, union)))
            case ARRAY =>
              val inner = indentedFp()(_.add("{").indent.call(printArrayValueInner(_, t, Some("x"))).outdent.add("}.toList"))
              s"case x: java.util.List[_] => this.$fieldName = ${cr.toConstructString(inner)}"
            case BYTES =>
              s"case x: java.nio.ByteBuffer => this.$fieldName = ${cr.toConstructString("x.array()")}"
            case RECORD | ENUM | FIXED =>
              s"case x: ${t.getFullName} => this.$fieldName = ${cr.toConstructString("x")}"
            case _ =>
              val typeName = simpleTypeToScalaReceiveType(t.getType)
              val x = if (t.getType == Type.STRING) "x.toString" else "x"
              val `case` = if (t.getType == Type.NULL) "x @ null" else s"x: $typeName"
              s"case ${`case`} => this.$fieldName = ${cr.toConstructString(x)}"
          }
        } :+ "case _ => throw new AvroRuntimeException(\"Invalid value\")"
      }.mkString("\n"))
      case OptionRepresentation(schema) =>
        printer
          .add(s"case null => this.$fieldName = None")
          .call { printer =>
            schema.getType match {
              case MAP => printOptionMapPatternMatch(printer, fieldName, schema, union)
              case ARRAY =>
                printer
                  .add(s"case x: java.util.List[_] => this.$fieldName = Some({")
                  .indent
                  .call(printArrayValueInner(_, schema, Some("x")))
                  .outdent
                  .add("}.toList)")
              case BYTES =>
                printer
                  .add(s"case x: java.nio.ByteBuffer => this.$fieldName = Some(x.array())")
              case RECORD | ENUM | FIXED =>
                printer
                  .add(s"case x: ${schema.getFullName} => this.$fieldName = Some(x)")
              case _ =>
                printer
                  .add(s"case x => this.$fieldName = Some(${toStringConverter("x", schema)}.asInstanceOf[${schemaToScalaType(schema)}])")
            }
          }
    }
  }

  private def printUnionValue(printer: FunctionalPrinter, union: UnionRepresentation): FunctionalPrinter = {
    union match {
      case cr: CoproductRepresentation => printer.add({
        cr.types.map { t =>
          if (t.getType == RECORD) s"case x: ${t.getFullName} => ${cr.toConstructString("x")}"
          else if (t.getType == MAP) printUnionMapValue(new FunctionalPrinter(), t, union).result()
          else {
            val typeName = simpleTypeToScalaReceiveType(t.getType)
            val x = if (t.getType == Type.STRING) "x.toString" else "x"
            val `case` = if (t.getType == Type.NULL) "x @ null" else s"x: $typeName"
            s"case ${`case`} => ${cr.toConstructString(x)}"
          }
        } :+ "case _ => throw new AvroRuntimeException(\"Invalid value\")"
      }.mkString("\n"))
      case OptionRepresentation(schema) =>
        printer.add(
          s"""case null => None
             |case x => Some(${toStringConverter("x", schema)}.asInstanceOf[${schemaToScalaType(schema)}])""".stripMargin
        )
    }
  }

  private def indentedFp(pattern: FunctionalPrinter = new FunctionalPrinter())(operate: FunctionalPrinter => FunctionalPrinter): String = {
    val inner = new FunctionalPrinter()
    inner.indent(pattern.indentLevel)
    operate(inner).result()
  }

  private def printUnionMapPatternMatch(functionalPrinter: FunctionalPrinter, fieldName: String, schema: Schema, union: UnionRepresentation): FunctionalPrinter = {
    val value = indentedFp(functionalPrinter) {
      _.indent
        .add("scala.jdk.CollectionConverters.MapHasAsScala(map).asScala.toMap map { kvp =>")
        .indent
        .add("val key = kvp._1.toString")
        .add("val value = kvp._2")
        .add(s"(key, {")
        .indent
        .call(printMapValueInner(_, schema.getValueType))
        .outdent
        .add("})")
        .outdent
        .add("}")
    }
    functionalPrinter.add(s"case map: java.util.Map[_,_] => this.$fieldName = ${union.toConstructString(value)}")
  }

  private def printOptionMapPatternMatch(functionalPrinter: FunctionalPrinter, fieldName: String, schema: Schema, union: UnionRepresentation): FunctionalPrinter = {
    functionalPrinter
      .add(s"case map: java.util.Map[_,_] => this.$fieldName = Some({")
      .indent
      .add("scala.jdk.CollectionConverters.MapHasAsScala(map).asScala.toMap map { kvp =>")
      .indent
      .add("val key = kvp._1.toString")
      .add("val value = kvp._2")
      .add(s"(key, {")
      .indent
      .call(printMapValueInner(_, schema.getValueType))
      .outdent
      .add("})")
      .outdent
      .add("}")
      .outdent
      .add("})")
  }

  private def printUnionMapValue(functionalPrinter: FunctionalPrinter, schema: Schema, union: UnionRepresentation): FunctionalPrinter = {
    val value = indentedFp(functionalPrinter) {
      _.indent
        .add("scala.jdk.CollectionConverters.MapHasAsScala(map).asScala.toMap map { kvp =>")
        .indent
        .add("val key = kvp._1.toString")
        .add("val value = kvp._2")
        .add(s"(key, {")
        .indent
        .call(printMapValueInner(_, schema.getValueType))
        .outdent
        .add("})")
        .outdent
        .add("}")
    }
    functionalPrinter.add(s"case map: java.util.Map[_,_] => ${union.toConstructString(value)}")
  }
}

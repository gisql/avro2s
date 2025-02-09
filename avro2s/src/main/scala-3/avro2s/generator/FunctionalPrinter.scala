package avro2s.generator

/**
 * This code is from the scalapb project - https://github.com/scalapb/ScalaPB.
 */

import FunctionalPrinter.PrinterEndo

object PrinterEndo {
  def apply(endo: PrinterEndo): PrinterEndo = endo
}

object FunctionalPrinter {
  type PrinterEndo = FunctionalPrinter => FunctionalPrinter
}

case class FunctionalPrinter(content: Vector[String] = Vector.empty, indentLevel: Int = 0) {
  val INDENT_SIZE = 2

  // Increase indent level
  def indent: FunctionalPrinter = indent(1)

  def indent(n: Int): FunctionalPrinter = copy(indentLevel = indentLevel + n)

  // Decreases indent level
  def outdent: FunctionalPrinter = outdent(1)

  def outdent(n: Int): FunctionalPrinter = {
    assert(indentLevel >= n)
    copy(indentLevel = indentLevel - n)
  }

  /** Adds strings at the current indent level. */
  def add(s: String*): FunctionalPrinter = {
    copy(
      content = content ++ s
        .flatMap(_.split("\n", -1))
        .map(l => " " * (indentLevel * INDENT_SIZE) + l)
    )
  }

  def seq(s: Seq[String]): FunctionalPrinter = add(s*)

  /** add with indent */
  def addIndented(s: String*): FunctionalPrinter = {
    this.indent.seq(s).outdent
  }

  /** apply the function with indent */
  def indented(f: PrinterEndo): FunctionalPrinter = this.indent.call(f).outdent

  def newline: FunctionalPrinter = add("")

  // Strips the margin, splits lines and adds.
  def addStringMargin(s: String): FunctionalPrinter =
    add(s.stripMargin)

  // Adds the strings, while putting a delimiter between two lines.
  def addWithDelimiter(delimiter: String)(s: Seq[String]) = {
    add(s.zipWithIndex.map { case (line, index) =>
      if (index == s.length - 1) line else (line + delimiter)
    }*)
  }

  def addGroupsWithDelimiter(delimiter: String)(groups: Seq[Seq[String]]) = {
    val lines = for {
      (group, index) <- groups.zipWithIndex
      (line, lineInGroup) <- group.zipWithIndex
    } yield
      if (index < groups.length - 1 && lineInGroup == group.length - 1)
        (line + delimiter)
      else line
    add(lines*)
  }

  def call(f: PrinterEndo*): FunctionalPrinter =
    f.foldLeft(this)((p, f) => f(p))

  def when(cond: => Boolean)(func: FunctionalPrinter => FunctionalPrinter) =
    if (cond) {
      func(this)
    } else {
      this
    }

  def print[M](
    objects: Iterable[M]
  )(f: (FunctionalPrinter, M) => FunctionalPrinter): FunctionalPrinter = {
    objects.foldLeft(this)(f)
  }

  def result() =
    content.mkString("\n")

  override def toString = s"FunctionalPrinter(lines=${content.length}, indentLevel=$indentLevel)"
}

package com.rtjfarrimond.sicpscheme

import com.rtjfarrimond.sicpscheme.ast.AbstractSyntaxTree

import scala.collection.mutable

sealed trait ParsingError extends Throwable {
  def message: String
}
case class IllegalStartOfExpression(found: Char) extends ParsingError {
  override def message: String = s"Expressions must start with '(', found '$found'"
}
case class IllegalEndOfExpression(found: Char) extends ParsingError {
  override def message: String = s"Expressions must end with ')', found '$found'"
}
case class UnknownOperator(found: Char) extends ParsingError {
  override def message: String = s"Unknown operator '$found'"
}
case class IntegerParseError(s: String) extends ParsingError {
  override def message: String = s"Could not parse integer from input '$s'"
}

object Parser {
  def parse(tokens: mutable.Queue[String]): AbstractSyntaxTree = ???
}

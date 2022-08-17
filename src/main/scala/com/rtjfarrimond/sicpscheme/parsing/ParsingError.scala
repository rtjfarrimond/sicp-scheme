package com.rtjfarrimond.sicpscheme.parsing

sealed trait ParsingError extends Throwable {
  def message: String
}

object ParsingError {
  
  case class UnknownOperator(found: String) extends ParsingError {
    override def message: String = s"Unknown operator '$found'"
  }

  case class TooFewArguments(atLeast: Int, found: Int) extends ParsingError {
    override def message: String = s"too few arguments (at least: $atLeast got: $found)"
  }

  case object InvalidExpression extends ParsingError {
    override def message: String = "Failed to parse expression"
  }

  case class IllegalStartOfExpression(found: Char) extends ParsingError {
    override def message: String = s"Expected '(' but found '$found'"
  }

  case class IllegalEndOfExpression(found: Char) extends ParsingError {
    override def message: String = s"Expected ')' but found '$found'"
  }

  case class FailedToParseInt(from: String) extends ParsingError {
    override def message: String = s"Failed to parse and integer from input '$from'"
  }

}
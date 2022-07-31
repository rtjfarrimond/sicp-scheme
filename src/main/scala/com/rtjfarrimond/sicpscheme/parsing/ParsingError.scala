package com.rtjfarrimond.sicpscheme.parsing

sealed trait ParsingError extends Throwable {
  def message: String
}

object ParsingError {
  case class IllegalStartOfExpression(found: Char) extends ParsingError {
    override def message: String = s"Expressions must start with '(', found '$found'"
  }

  case class IllegalEndOfExpression(found: Char) extends ParsingError {
    override def message: String = s"Expressions must end with ')', found '$found'"
  }

  case object UnmatchedRightParenthesis extends ParsingError {
    override def message: String = s"Unmatched ')'"
  }

  case class UnknownOperator(found: String) extends ParsingError {
    override def message: String = s"Unknown operator '$found'"
  }

  case class IntegerParseError(s: String) extends ParsingError {
    override def message: String = s"Could not parse integer from input: $s"
  }

  case class TooFewArguments(atLeast: Int, found: Int) extends ParsingError {
    override def message: String = s"too few arguments (at least: $atLeast got: $found)"
  }

  case object InvalidExpression extends ParsingError {
    override def message: String = "Failed to parse expression"
  }
}
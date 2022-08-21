package com.rtjfarrimond.sicpscheme.parsing

import cats.Show

sealed trait ParsingError extends Throwable {
  def message: String
}

object ParsingError {

  sealed trait SchemeError extends ParsingError
  sealed trait SyntaxError extends ParsingError

  implicit val show: Show[ParsingError] = new Show[ParsingError] {
    def show(err: ParsingError): String =
      err match {
        case _: SchemeError => s"SchemeError: ${err.message}\n"
        case _: SyntaxError => s"SyntaxError: ${err.message}\n"
      }
  }

  case class TooFewArguments(atLeast: Int, found: Int) extends SchemeError {
    override def message: String = s"too few arguments (at least: $atLeast got: $found)"
  }

  case object InvalidExpression extends SchemeError {
    override def message: String = "Failed to parse expression"
  }

  // TODO: Bring this in line with Berkley interpreter
  case class IllegalStartOfExpression(found: Char) extends SyntaxError {
    override def message: String = s"Expected '(' but found '$found'"
  }

  case object UnexpectedEndOfFile extends SyntaxError {
    override def message: String = "unexpected end of file"
  }

  case class UnknownIdentifier(identifier: String) extends SchemeError {
    override def message: String = s"unknown identifier: $identifier"
  }

}
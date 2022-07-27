package com.rtjfarrimond.sicpscheme

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.*

import scala.annotation.tailrec
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
case object UnmatchedRightParenthesis extends ParsingError {
  override def message: String = s"Unmatched ')'"
}
case class UnknownOperator(found: String) extends ParsingError {
  override def message: String = s"Unknown operator '$found'"
}
case class IntegerParseError(s: String) extends ParsingError {
  override def message: String = s"Could not parse integer from input '$s'"
}
case class TooFewArguments(atLeast: Int, found: Int) extends ParsingError {
  override def message: String = s"too few arguments (at least: $atLeast got: $found)"
}

object Parser {
  def parse(tokens: mutable.Queue[String]): Either[ParsingError, AbstractSyntaxTree] = {
    @tailrec
    def loop(
              openParensCount: Int,
              buffer: List[String],
              tokens: mutable.Queue[String]
    ): Either[ParsingError, AbstractSyntaxTree] = {
      if (openParensCount == 0 && tokens.head != "(")
        Left(IllegalStartOfExpression(tokens.head.head))
      else {
        tokens.dequeue() match {
          case "(" =>
            loop(openParensCount + 1, buffer, tokens)
          case ")" if openParensCount == 0 =>
            Left(UnmatchedRightParenthesis)
          case s if !Set("(", ")").contains(s) =>
            loop(openParensCount, buffer.prepended(s), tokens)
          case ")" =>
            parseExpression(buffer)
          case s =>
            loop(openParensCount, buffer.prepended(s), tokens)
        }
      }
    }

    loop(0, List.empty, tokens)
  }

  private def parseExpression(buffer: List[String]): Either[ParsingError, AbstractSyntaxTree] = {
    buffer.indexWhere(Lexer.numericOperatorTokens.map(_.toString).contains(_)) match {
      case -1 => ???
      case i =>
        val literals = buffer
          .take(i)
          .reverse
          .map(_.toInt) // TODO: Handle int parse error
          .map(Literal.apply)
        buffer(i) match {
          case "+" if literals.nonEmpty =>
            Right(Plus(NonEmptyList(literals.head, literals.tail)))
          case "+" =>
            Right(Plus.unit)
          case "-" if literals.nonEmpty =>
            Right(Minus(NonEmptyList(literals.head, literals.tail)))
          case "-" =>
            Left(TooFewArguments(1, 0))
          case "*" if literals.nonEmpty =>
            Right(Multiply(NonEmptyList(literals.head, literals.tail)))
          case "*" =>
            Right(Multiply.unit)
          case "/" if literals.nonEmpty =>
            Right(Divide(NonEmptyList(literals.head, literals.tail)))
          case "/" =>
            Left(TooFewArguments(1, 0))
          case s =>
            Left(UnknownOperator(s))
        }
    }
  }
}

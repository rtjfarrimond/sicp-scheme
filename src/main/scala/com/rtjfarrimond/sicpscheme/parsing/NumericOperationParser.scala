package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.{Divide, Literal, Minus, Multiply, NumericOperation, Plus}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.{ TooFewArguments, UnknownOperator }

object NumericOperationParser {
  def parse(str: String, literals: List[Literal]): Either[ParsingError, NumericOperation] =
    str match {
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

package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Divide, Literal, Minus, Multiply, NumericOperation, Plus}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.{FailedToParseNumericOperation, TooFewArguments, UnknownOperator}

object NumericOperationParser {
  def parse(
    str: String,
    children: List[AbstractSyntaxTree]
  ): Either[FailedToParseNumericOperation, NumericOperation] = {
    str match {
      case "+" => Right {
        if (children.nonEmpty) Plus(NonEmptyList(children.head, children.tail))
        else Plus.unit
      }
      case "*" => Right {
        if (children.nonEmpty) Multiply(NonEmptyList(children.head, children.tail))
        else Multiply.unit
      }
      case "-" if children.nonEmpty =>
        Right(Minus(NonEmptyList(children.head, children.tail)))
      case "/" if children.nonEmpty =>
        Right(Divide(NonEmptyList(children.head, children.tail)))
      case s =>
        Left(FailedToParseNumericOperation(s))
    }
  }
}
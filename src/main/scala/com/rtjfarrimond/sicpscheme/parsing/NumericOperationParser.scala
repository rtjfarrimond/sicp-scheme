package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Divide, Literal, Minus, Multiply, NumericOperation, Plus}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.{TooFewArguments, UnknownOperator}

object NumericOperationParser {
  def parse(str: String, children: List[AbstractSyntaxTree]): Either[ParsingError, NumericOperation] = {
    str match {
      case "+" if children.nonEmpty =>
        Right(Plus(NonEmptyList(children.head, children.tail)))
      case "+" =>
        Right(Plus.unit)
      case "-" if children.nonEmpty =>
        Right(Minus(NonEmptyList(children.head, children.tail)))
      case "-" =>
        Left(TooFewArguments(1, 0))
      case "*" if children.nonEmpty =>
        Right(Multiply(NonEmptyList(children.head, children.tail)))
      case "*" =>
        Right(Multiply.unit)
      case "/" if children.nonEmpty =>
        Right(Divide(NonEmptyList(children.head, children.tail)))
      case "/" =>
        Left(TooFewArguments(1, 0))
      case s =>
        Left(UnknownOperator(s))
    }
  }

  def newParse(str: String, children: List[AbstractSyntaxTree]): NumericOperation = {
    str match {
      case "+" if children.nonEmpty =>
        Plus(NonEmptyList(children.head, children.tail))
      case "+" =>
        Plus.unit
      case "-" if children.nonEmpty =>
        Minus(NonEmptyList(children.head, children.tail))
      case "*" if children.nonEmpty =>
        Multiply(NonEmptyList(children.head, children.tail))
      case "*" =>
        Multiply.unit
      case "/" if children.nonEmpty =>
        Divide(NonEmptyList(children.head, children.tail))
      case s =>
        throw new RuntimeException("BANG!")
    }
  }

}

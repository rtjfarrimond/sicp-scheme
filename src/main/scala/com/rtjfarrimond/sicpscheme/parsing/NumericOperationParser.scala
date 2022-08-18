package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Divide, Literal, Minus, Multiply, NumericOperation, Plus}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.{TooFewArguments, UnknownOperator}

object NumericOperationParser {
  // TODO: Tests for this
  def parse(str: String, children: List[AbstractSyntaxTree]): NumericOperation = {
    str match {
      case "+" =>
        if (children.nonEmpty) Plus(NonEmptyList(children.head, children.tail))
        else Plus.unit
      case "*" =>
        if (children.nonEmpty) Multiply(NonEmptyList(children.head, children.tail))
        else Multiply.unit
      case "-" if children.nonEmpty =>
        Minus(NonEmptyList(children.head, children.tail))
      case "/" if children.nonEmpty =>
        Divide(NonEmptyList(children.head, children.tail))
      case s =>
        throw new RuntimeException("BANG!") // TODO: Make this better
    }
  }
}
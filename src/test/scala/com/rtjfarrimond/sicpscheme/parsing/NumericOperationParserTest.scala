package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.*
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.FailedToParseNumericOperation
import munit.FunSuite

class NumericOperationParserTest extends FunSuite {

  test("parse the unit of addition") {
    val actual = NumericOperationParser.parse("+", Nil)
    val expected = Right(Plus.unit)
    assertEquals(actual, expected)
  }

  test("parse the unit of multiplication") {
    val actual = NumericOperationParser.parse("*", Nil)
    val expected = Right(Multiply.unit)
    assertEquals(actual, expected)
  }

  test("parse an addition") {
    val literals = List(Literal(11), Literal(31))
    val actual = NumericOperationParser.parse("+", literals)
    val expected = Plus(literals)
    assertEquals(actual, Right(expected))
  }

  test("parse a multiplication") {
    val literals = List(Literal(21), Literal(2))
    val actual = NumericOperationParser.parse("*", literals)
    val expected = Multiply(literals)
    assertEquals(actual, Right(expected))
  }

  test("parse a subtraction") {
    val literals = NonEmptyList.of(Literal(44), Literal(2))
    val actual = NumericOperationParser.parse("-", literals.toList)
    val expected = Right(Minus(literals))
    assertEquals(actual, expected)
  }

  test("parse a division") {
    val literals = NonEmptyList.of(Literal(84), Literal(2))
    val actual = NumericOperationParser.parse("/", literals.toList)
    val expected = Right(Divide(literals))
    assertEquals(actual, expected)
  }

  test("Left when failed to parse an operation") {
    val input = "foo"
    val actual = NumericOperationParser.parse(input, Nil)
    val expected = Left(FailedToParseNumericOperation(input))
    assertEquals(actual, expected)
  }

}

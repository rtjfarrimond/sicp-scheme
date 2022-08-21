package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.*
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.UnknownIdentifier
import munit.FunSuite

class NumericParserTest extends FunSuite {

  test("getOutermostExpressionTokens must return all between matching parens") {
    val input = List("(", "+", "11", "(", "+", "15", "16", ")", ")", "1", ")")
    val tokens = List.from(input)

    val actual = NumericParser.getFirstOutermostExpressionTokens(tokens)
    val expected = (List("+", "11", "(", "+", "15", "16", ")"), List("1", ")"))

    assertEquals(actual, expected)
  }

  test("parse the unit of addition") {
    val input = List("(", "+", ")")
    val actual = NumericParser.parseNumericOperation(input)
    val expected = Right(Plus.unit)
    assertEquals(actual, expected)
  }

  test("parse the unit of multiplication") {
    val input = List("(", "*", ")")
    val actual = NumericParser.parseNumericOperation(input)
    val expected = Right(Multiply.unit)
    assertEquals(actual, expected)
  }

  test("parse an addition") {
    val input = List("(", "+", "11", "31", ")")
    val actual = NumericParser.parseNumericOperation(input)
    val expected = Plus(List(NumericLiteral(11), NumericLiteral(31)))
    assertEquals(actual, Right(expected))
  }

  test("parse a multiplication") {
    val input = List("(", "*", "21", "2", ")")
    val actual = NumericParser.parseNumericOperation(input)
    val expected = Multiply(List(NumericLiteral(21), NumericLiteral(2)))
    assertEquals(actual, Right(expected))
  }

  test("parse a subtraction") {
    val input = List("(", "-", "44", "2", ")")
    val actual = NumericParser.parseNumericOperation(input)
    val expected = Right(Minus(NonEmptyList.of(NumericLiteral(44), NumericLiteral(2))))
    assertEquals(actual, expected)
  }

  test("parse a division") {
    val input = List("(", "/", "84", "2", ")")
    val actual = NumericParser.parseNumericOperation(input)
    val expected = Right(Divide(NonEmptyList.of(NumericLiteral(84), NumericLiteral(2))))
    assertEquals(actual, expected)
  }

  test("Left when failed to parse an operation") {
    val foo = "foo"
    val actual = NumericParser.parseNumericOperation(List(foo))
    val expected = Left(UnknownIdentifier(foo))
    assertEquals(actual, expected)
  }

}

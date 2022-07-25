package com.rtjfarrimond.sicpscheme

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.*
import munit.FunSuite

import scala.collection.mutable

class ParserTest extends FunSuite {

  test("IllegalStartOfExpression when expression does not begin with '('") {
    val tokens = mutable.Queue("+", ")")

    val expected = IllegalStartOfExpression('+')
    val result = Parser.parse(tokens)
    assert(result.isLeft)
    val Left(actual) = result
    assertEquals(actual, expected)
  }

  test("Must parse the unit of addition") {
    val tokens = mutable.Queue("(", "+", ")")

    val expected: AbstractSyntaxTree = Plus.unit
    val result = Parser.parse(tokens)
    assert(result.isRight)
    val Right(actual) = result
    assertEquals(expected, actual)
    assertEquals(actual.value, 0)
  }

  test("Must parse the unit of multiplication") {
    val tokens = mutable.Queue("(", "*", ")")

    val expected: AbstractSyntaxTree = Multiply.unit
    val result = Parser.parse(tokens)
    assert(result.isRight)
    val Right(actual) = result
    assertEquals(expected, actual)
    assertEquals(actual.value, 1)
  }

  test("TooFewArguments for division with 0 args") {
    val tokens = mutable.Queue("(", "/", ")")

    val expected = TooFewArguments(1, 0)
    val result = Parser.parse(tokens)
    assert(result.isLeft)
    val Left(actual) = result
    assertEquals(actual, expected)
  }

  test("TooFewArguments for subtraction with 0 args") {
    val tokens = mutable.Queue("(", "-", ")")

    val expected = TooFewArguments(1, 0)
    val result = Parser.parse(tokens)
    assert(result.isLeft)
    val Left(actual) = result
    assertEquals(actual, expected)
  }

  test("Parse a simple addition") {
    val tokens = mutable.Queue("(", "+", "11", "31", ")")

    val expected = Plus(NonEmptyList.of(Literal(11), Literal(31)))
    val result = Parser.parse(tokens)
    assert(result.isRight)
    val Right(actual) = result
    assertEquals(actual, expected)
  }

}

package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.*
import com.rtjfarrimond.sicpscheme.parsing.Parser
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.{IllegalStartOfExpression, InvalidExpression, TooFewArguments}
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

  test("InvalidExpression when no operator specified") {
    val tokens = mutable.Queue("(", "43", "1", ")")

    val expected = InvalidExpression
    val result = Parser.parse(tokens)
    assert(result.isLeft)
    val Left(actual) = result
    assertEquals(actual, expected)
  }

  test("Blow up when failing to parse Ints") {
    val tokens = mutable.Queue("(", "+", "1", "BANG!", ")")
    val result = Parser.parse(tokens)

    assert(result.isLeft)
    val Left(err) = result
    assertEquals(err.message, "Could not parse integer from input: For input string: \"BANG!\"")
  }

  test("Parse a simple addition") {
    val tokens = mutable.Queue("(", "+", "11", "31", ")")

    val expected = Plus(NonEmptyList.of(Literal(11), Literal(31)))
    val result = Parser.parse(tokens)
    assert(result.isRight)
    val Right(actual) = result
    assertEquals(actual, expected)
  }

  test("Parse a recursive addition") {
    val tokens = mutable.Queue("(", "+", "11", "(", "+", "15", "16", ")")

    val expected = Plus(NonEmptyList.of(Literal(11), Plus(NonEmptyList.of(Literal(15), Literal(16)))))
    val result = Parser.parse(tokens)
    assert(result.isRight)
    val Right(actual) = result
    assertEquals(actual, expected)
  }

  test("Parse nested additions") {
    val tokens = mutable.Queue("(", "+", "(", "+", "5", "6", ")", "(", "+", "15", "16", ")", ")")

    val left = Plus(NonEmptyList.of(Literal(4), Literal(7)))
    val right = Plus(NonEmptyList.of(Literal(15), Literal(16)))
    val expected = Plus(NonEmptyList.of(left, right))
    val result = Parser.parse(tokens)
    assert(result.isRight)
    val Right(actual) = result
    assertEquals(actual, expected)
  }

}

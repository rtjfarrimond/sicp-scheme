package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast._
import com.rtjfarrimond.sicpscheme.parsing.AstParser
import com.rtjfarrimond.sicpscheme.parsing.ParsingError._
import munit.FunSuite

class AstParserTest extends FunSuite {

  test("parse an integer literal") {
    val tokens = NonEmptyList.one("42")

    val actual = AstParser.parseAst(tokens)

    val expected = Right(NumericLiteral(42))
    assertEquals(actual, expected)
  }

  test("Parse a simple addition") {
    val tokens = NonEmptyList.of("(", "+", "15", "16", "11", ")")

    val actual = AstParser.parseAst(tokens)

    val expected = Right(Plus(List(15, 16, 11).map(NumericLiteral.apply)))
    assertEquals(actual, expected)
  }

  test("Parse a recursive addition") {
    val tokens = NonEmptyList.of("(", "+", "11", "(", "+", "15", "16", ")", ")")

    val actual = AstParser.parseAst(tokens)

    val expected = Right(Plus(NonEmptyList.of(NumericLiteral(11), Plus(NonEmptyList.of(NumericLiteral(15), NumericLiteral(16))))))
    assertEquals(actual, expected)
  }

  test("Parse nested additions") {
    val tokens = NonEmptyList.of("(", "+", "(", "+", "5", "6", ")", "(", "+", "15", "16", ")", ")")

    val actual = AstParser.parseAst(tokens)

    val lhs = Plus(NonEmptyList.of(NumericLiteral(5), NumericLiteral(6)))
    val rhs = Plus(NonEmptyList.of(NumericLiteral(15), NumericLiteral(16)))
    val expected = Right(Plus(NonEmptyList.of(lhs, rhs)))
    assertEquals(actual, expected)
  }

  test("Parse the unit of addition") {
    val tokens = NonEmptyList.of("(", "+", ")")
    val actual = AstParser.parseAst(tokens)
    assertEquals(actual, Right(Plus.unit))
  }

  test("Parse the unit of multiplication") {
    val tokens = NonEmptyList.of("(", "*", ")")
    val actual = AstParser.parseAst(tokens)
    assertEquals(actual, Right(Multiply.unit))
  }

  // TODO: This is the current behaviour, but is not in line with Berkely interpreter, needs to be changed but not small
  test("error given expression missing opening paren") {
    val actual = AstParser.parseAst(NonEmptyList.of("+", "11", "31", ")"))
    assertEquals(actual, Left(UnknownIdentifier("+")))
  }

  test("error given expression missing opening paren in nested expression") {
    val actual = AstParser.parseAst(NonEmptyList.of("(", "+", "7", "31", ")", "4", ")"))
    assertEquals(actual, Left(IllegalStartOfExpression('4')))
  }

  test("error given expression missing closing paren") {
    val actual = AstParser.parseAst(NonEmptyList.of("(", "+", "11", "31"))
    assertEquals(actual, Left(UnexpectedEndOfFile))
  }

  test("error when fail to parse integer") {
    val notAnInt = "abc"
    val actual = AstParser.parseAst(NonEmptyList.of("(", "+", "11", notAnInt, ")"))
    assertEquals(actual, Left(UnknownIdentifier(notAnInt)))
  }

}

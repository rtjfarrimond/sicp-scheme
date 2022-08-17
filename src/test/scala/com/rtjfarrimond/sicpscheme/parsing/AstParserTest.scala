package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast._
import com.rtjfarrimond.sicpscheme.parsing.AstParser
import com.rtjfarrimond.sicpscheme.parsing.ParsingError._
import munit.FunSuite

class AstParserTest extends FunSuite {

  test("getOutermostExpressionTokens must return all between matching parens") {
    val input = List("(", "+", "11", "(", "+", "15", "16", ")", ")", "1", ")")
    val tokens = List.from(input)

    val actual = AstParser.getFirstOutermostExpressionTokens(tokens)
    val expected = (List("+", "11", "(", "+", "15", "16", ")"), List("1", ")"))

    assertEquals(actual, expected)
  }

  test("Parse a simple addition") {
    val tokens = List("(", "+", "15", "16", "11", ")")

    val actual = AstParser.parseAst(tokens)

    val expected = Right(Plus(List(15, 16, 11).map(Literal.apply)))
    assertEquals(actual, expected)
  }

  test("Parse a recursive addition") {
    val tokens = List("(", "+", "11", "(", "+", "15", "16", ")", ")")

    val actual = AstParser.parseAst(tokens)

    val expected = Right(Plus(NonEmptyList.of(Literal(11), Plus(NonEmptyList.of(Literal(15), Literal(16))))))
    assertEquals(actual, expected)
  }

  test("Parse nested additions") {
    val tokens = List("(", "+", "(", "+", "5", "6", ")", "(", "+", "15", "16", ")", ")")

    val actual = AstParser.parseAst(tokens)

    val lhs = Plus(NonEmptyList.of(Literal(5), Literal(6)))
    val rhs = Plus(NonEmptyList.of(Literal(15), Literal(16)))
    val expected = Right(Plus(NonEmptyList.of(lhs, rhs)))
    assertEquals(actual, expected)
  }

  test("Parse the unit of addition") {
    val tokens = List("(", "+", ")")
    val actual = AstParser.parseAst(tokens)
    assertEquals(actual, Right(Plus.unit))
  }

  test("Parse the unit of multiplication") {
    val tokens = List("(", "*", ")")
    val actual = AstParser.parseAst(tokens)
    assertEquals(actual, Right(Multiply.unit))
  }

  test("error given empty list as input") {
    val actual = AstParser.parseAst(Nil)
    assertEquals(actual, Left(InvalidExpression))
  }

  test("error given expression missing opening paren") {
    val actual = AstParser.parseAst(List("+", "11", "31", ")"))
    assertEquals(actual, Left(IllegalStartOfExpression('+')))
  }

  test("error given expression missing opening paren in nested expression") {
    val actual = AstParser.parseAst(List("(", "+", "7", "31", ")", "4", ")"))
    assertEquals(actual, Left(IllegalStartOfExpression('4')))
  }

  test("error given expression missing closing paren") {
    val actual = AstParser.parseAst(List("(", "+", "11", "31"))
    assertEquals(actual, Left(IllegalEndOfExpression('1')))
  }

  test("error when fail to parse integer") {
    val notAnInt = "abc"
    val actual = AstParser.parseAst(List("(", "+", "11", notAnInt, ")"))
    assertEquals(actual, Left(FailedToParseInt(notAnInt)))
  }

}

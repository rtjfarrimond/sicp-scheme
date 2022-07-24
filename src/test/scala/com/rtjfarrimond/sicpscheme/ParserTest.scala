package com.rtjfarrimond.sicpscheme

import munit.FunSuite

import scala.collection.mutable

class ParserTest extends FunSuite {

  test("Must parse a single expression") {
    val tokens = mutable.Queue("(", "+", "23", "19", ")")

    val expected = Expression(Plus, 23, 19)
    val result = Parser.parse(tokens).head
    assert(result.isRight)

    val Right(actual) = result
    assertEquals(expected, actual)
  }

  test("Must parse nested expressions") {
    val tokens = mutable.Queue("(", "+", "24", "(", "*", "2", "9", ")", ")")
    val expected = Expression(Plus, 24, Expression(Multiply, 2, 9))

    val result = Parser.parse(tokens).head
    println(result)
    assert(result.isRight)
    val Right(actual) = result
    assertEquals(expected, actual)
  }

  test("Must parse multiple un-nested expressions") {
    val tokens = mutable.Queue("(", "+", "(", "+", "15", "17", ")", "(", "+", "2", "8", ")", ")")
    val expected = Expression(Plus, Expression(Plus, 15, 17), Expression(Plus, 2, 7))

    val result = Parser.parse(tokens).head
    assert(result.isRight)
    val Right(actual) = result
    assertEquals(expected, actual)
  }

}

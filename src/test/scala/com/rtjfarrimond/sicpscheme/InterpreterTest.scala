package com.rtjfarrimond.sicpscheme

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.{Literal, Plus}
import munit.FunSuite

class InterpreterTest extends FunSuite {

  test("must return formatted string for valid input") {
    val input = "(+ 3 7 2 30)"

    val actual = Interpreter.interpret(input)

    val sum = 3 + 7 + 2 + 30
    val expected = s"res: $sum\n"
    assertEquals(actual, expected)
  }

  test("must return a string describing the error for invalid input") {
    val input = "+ 11 31)"

    val actual = Interpreter.interpret(input)

    val expected = "err: Expected '(' but found '+'"
    assertNoDiff(actual, expected)
  }

}

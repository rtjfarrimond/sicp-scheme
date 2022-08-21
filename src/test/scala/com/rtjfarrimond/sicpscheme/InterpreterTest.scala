package com.rtjfarrimond.sicpscheme

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.{NumericLiteral, Plus}
import munit.FunSuite

class InterpreterTest extends FunSuite {

  test("must return formatted string for valid expression") {
    val input = "(+ 3 7 2 30)"

    val actual = Interpreter.interpret(input)

    val sum = 3 + 7 + 2 + 30
    val expected = s"res: $sum\n"
    assertEquals(actual, expected)
  }

  test("must return the empty string given the empty string as input") {
    val actual = Interpreter.interpret("")
    assertEquals(actual, "")
  }

  test("must return formatted string for primitive procedure") {
    val actual = Interpreter.interpret("+")
    val expected = "res: PrimitiveProcedure\n"
    assertEquals(actual, expected)
  }

  test("must return a string describing the error for invalid input") {
    val input = "(+ 11 31"

    val actual = Interpreter.interpret(input)

    val expected = "SyntaxError: unexpected end of file\n"
    assertNoDiff(actual, expected)
  }

}

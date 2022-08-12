package com.rtjfarrimond.sicpscheme

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.{Literal, Plus}
import munit.FunSuite

class InterpreterTest extends FunSuite {

  test("interpret must interpret the command and return a formatted string with the result") {
    val input = "(+ 3 7 2 30)"

    val actual = Interpreter.interpret(input)

    val sum = 3 + 7 + 2 + 30
    val expected = s"res: $sum\n"
    assertEquals(actual, expected)
  }

}

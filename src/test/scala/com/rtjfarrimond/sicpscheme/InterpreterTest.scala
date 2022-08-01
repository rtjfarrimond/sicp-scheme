package com.rtjfarrimond.sicpscheme

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.{Literal, Plus}
import munit.FunSuite

class InterpreterTest extends FunSuite {

  test("run must compute the value of an AST") {
    val lhs = Plus(List(Literal(3), Literal(7)))
    val rhs = Plus(List(Literal(2), Literal(30)))
    val input = Plus(List(lhs, rhs))

    val actual = Interpreter.run(input)

    val expected = 3 + 7 + 2 + 30
    assertEquals(actual, expected)
  }

}

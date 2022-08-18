package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast.*
import munit.FunSuite

class NumericOperationParserTest extends FunSuite {

  test("parse the unit of addition") {
    val actual = NumericOperationParser.parse("+", Nil)
    assertEquals(actual, Plus.unit)
  }

  test("parse the unit of multiplication") {
    val actual = NumericOperationParser.parse("*", Nil)
    assertEquals(actual, Multiply.unit)
  }

  test("parse an addition") {
    val literals = List(Literal(11), Literal(31))
    val actual = NumericOperationParser.parse("+", literals)
    assertEquals(actual, Plus(literals))
  }

  test("parse a multiplication") {
    val literals = List(Literal(21), Literal(2))
    val actual = NumericOperationParser.parse("*", literals)
    assertEquals(actual, Multiply(literals))
  }

  test("parse a subtraction") {
    val literals = NonEmptyList.of(Literal(44), Literal(2))
    val actual = NumericOperationParser.parse("-", literals.toList)
    assertEquals(actual, Minus(literals))
  }

  test("parse a division") {
    val literals = NonEmptyList.of(Literal(84), Literal(2))
    val actual = NumericOperationParser.parse("/", literals.toList)
    assertEquals(actual, Divide(literals))
  }

  // TODO: Better name
  test("foo") {
    val actual = NumericOperationParser.parse("f", Nil)
    assert(false)
  }

}

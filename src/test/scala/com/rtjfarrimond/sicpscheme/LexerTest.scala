package com.rtjfarrimond.sicpscheme

import munit.FunSuite

class LexerTest extends FunSuite {

  test("tokenize must work with whitespace everywhere") {
    val expr = "( + 23 19 )"
    val expected = List("(", "+", "23", "19", ")")
    assertEquals(Lexer.tokenize(expr), expected)
  }

  test("tokenize must work with whitespace between operators and operands") {
    val expr = "(+ 23 19)"
    val expected = List("(", "+", "23", "19", ")")
    assertEquals(Lexer.tokenize(expr), expected)
  }

  test("tokenize must work with whitespace between operands") {
    val expr = "(+23 19)"
    val expected = List("(", "+", "23", "19", ")")
    assertEquals(Lexer.tokenize(expr), expected)
  }

  test("tokenize must work with multiple operations") {
    val expr = "(+24 (* 2 9))"
    val expected = List("(", "+", "24", "(", "*", "2", "9", ")", ")")
    assertEquals(Lexer.tokenize(expr), expected)
  }

}

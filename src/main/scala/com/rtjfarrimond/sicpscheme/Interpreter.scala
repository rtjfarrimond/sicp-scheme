package com.rtjfarrimond.sicpscheme

import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Leaf, Literal, Node}

object Interpreter {
  // TODO: Test for unhappy path
  def interpret(s: String): String = {
    val tokens = Lexer.tokenize(s)
    val ast = parsing.AstParser.parseAst(tokens)
    ast match {
      case Left(errs) => s"err: ${errs.message}\n"
      case Right(ast) => s"res: ${ast.value.toString}\n"
    }
  }
}

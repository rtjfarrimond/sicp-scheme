package com.rtjfarrimond.sicpscheme

import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Leaf, Literal, Node}

object Interpreter {
  def interpret(s: String): String = {
    val tokens = Lexer.tokenize(s)
    parsing.AstParser.parseAst(tokens) match {
      case Left(err) => s"err: ${err.message}\n"
      case Right(ast) => s"res: ${ast.value.toString}\n"
    }
  }
}

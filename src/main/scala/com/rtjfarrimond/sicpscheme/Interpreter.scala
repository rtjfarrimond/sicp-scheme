package com.rtjfarrimond.sicpscheme

import cats.data.NonEmptyList
import cats.syntax.all._
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Leaf, Node, NumericLiteral, PrimitiveProcedure}
import com.rtjfarrimond.sicpscheme.parsing.NumericParser
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.UnknownIdentifier

object Interpreter {
  def interpret(s: String): String = {
    Lexer.tokenize(s) match {
      case Nil => ""
      case tokens =>
        val nelTokens = NonEmptyList(tokens.head, tokens.tail)
        parsing.AstParser.parseAst(nelTokens) match {
          case Left(err) => err.show
          case Right(ast) => s"res: ${ast.stringValue}\n" // TODO: DRY this
        }
    }
  }
}

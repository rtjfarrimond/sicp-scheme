package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import cats.syntax.all._
import com.rtjfarrimond.sicpscheme.Lexer
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Numeric, NumericLiteral, NumericOperation, PrimitiveProcedure}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.*

import scala.annotation.tailrec

object AstParser {

  def parseAst(tokens: NonEmptyList[String]): Either[ParsingError, AbstractSyntaxTree] =
    tokens.toList match {
      case str :: Nil =>
        PrimitiveProcedure.ofChar(str.head) match {
          case None =>
            NumericParser.parseNumericLiteral(str)
          case Some(pp) =>
            Right(pp)
        }
      case tokensList =>
        NumericParser.parseNumericOperation(tokensList)
    }
  
}

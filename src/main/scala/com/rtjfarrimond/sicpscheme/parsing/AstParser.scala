package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.Lexer
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Literal}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError._

import scala.annotation.tailrec

object AstParser {

  def parseAst(tokens: List[String]): Either[ParsingError, AbstractSyntaxTree] =
    tokens match {
      case Nil =>
        Left(InvalidExpression)
      case head :: Nil =>
        Literal.fromString(head) match {
          case None =>
            Left(FailedToParseInt(head))
          case Some(literal) =>
            Right(literal)
        }
      case firstString :: _ if firstString.head != '(' =>
        Left(IllegalStartOfExpression(firstString.head))
      case _ =>
        val lastChar = tokens.last.last
        if (lastChar != ')') Left(IllegalEndOfExpression(lastChar))
        else {
          val (exprTokens, rest) = getFirstOutermostExpressionTokens(tokens)
          if (rest.nonEmpty) Left(IllegalStartOfExpression(rest.head.head))
          else {
            parseChildren(exprTokens.tail, List.empty).flatMap { children =>
              NumericOperationParser.parse(exprTokens.head, children)
            }
          }
        }
    }

  @tailrec
  private def parseChildren(tokens: List[String], acc: List[AbstractSyntaxTree]): Either[ParsingError, List[AbstractSyntaxTree]] = {
    if (tokens.isEmpty) Right(acc)
    else tokens.head match {
      case "(" =>
        val (exprTokens, rest) = getFirstOutermostExpressionTokens(tokens)
        parseAst(exprTokens.prepended("(").appended(")")) match {
          case Left(pe) => Left(pe)
          case Right(ast) => parseChildren(rest, acc :+ ast)
        }
      case s =>
        Literal.fromString(s) match {
          case None =>
            Left(FailedToParseInt(s))
          case Some(literal) =>
            parseChildren(tokens.tail, acc :+ literal)
        }
    }
  }

  private[parsing] def getFirstOutermostExpressionTokens(
    tokens: List[String]
  ): (List[String], List[String]) = {
    @tailrec
    def loop(openParensCount: Int, acc: List[String], rest: List[String]): (List[String], List[String]) = {
      rest.head match {
        case "(" if openParensCount == 0 =>
          loop(openParensCount + 1, acc, rest.tail)
        case s @ "(" =>
          loop(openParensCount + 1, acc.appended(s), rest.tail)
        case ")" if openParensCount == 1 =>
          (acc, rest.tail)
        case s @ ")" =>
          loop(openParensCount - 1, acc.appended(s), rest.tail)
        case s =>
          loop(openParensCount, acc.appended(s), rest.tail)
      }
    }
    loop(openParensCount = 0, List.empty, tokens)
  }

}

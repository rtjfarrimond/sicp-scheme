package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.Lexer
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Literal}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.*

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object AstParser {

  def parseAst(tokens: List[String]): Either[ParsingError, AbstractSyntaxTree] = {
    val (exprTokens, rest) = getFirstOutermostExpressionTokens(tokens)
    if (rest.nonEmpty) Left(InvalidExpression) // TODO: More detailed error & test for this case
    else {
      parseChildren(exprTokens.tail, List.empty).map { children =>
        NumericOperationParser.newParse(exprTokens.head, children)
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
        parseChildren(tokens.tail, acc :+ Literal(s.toInt))
    }
  }

  private[parsing] def getFirstOutermostExpressionTokens(tokens: List[String]): (List[String], List[String]) = {
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

package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.Lexer
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Literal}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.*

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Parser {

  def parseExpression(tokens: List[String]): AbstractSyntaxTree = {
    val (exprTokens, _) = getOutermostExpressionTokens(tokens) // TODO: What happens when rest is non-empty?
    NumericOperationParser.newParse(exprTokens.head, parseChildren(exprTokens.tail))
  }

  private def parseChildren(tokens: List[String]): List[AbstractSyntaxTree] = {
    @tailrec
    def loop(tokens: List[String], acc: List[AbstractSyntaxTree]): List[AbstractSyntaxTree] = {
      if (tokens.isEmpty) acc
      else tokens.head match {
        case "(" =>
          val (exprTokens, rest) = getOutermostExpressionTokens(tokens)
          loop(rest, acc :+ parseExpression(exprTokens.prepended("(").appended(")")))
        case s =>
          loop(tokens.tail, acc :+ Literal(s.toInt))
      }
    }
    loop(tokens, List.empty)
  }

  private[parsing] def getOutermostExpressionTokens(tokens: List[String]): (List[String], List[String]) = {
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

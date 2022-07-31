package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.Lexer
import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Literal}
import com.rtjfarrimond.sicpscheme.parsing.ParsingError.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Parser {

  def newParse(tokens: mutable.Queue[String]): Either[ParsingError, AbstractSyntaxTree] = {
    def loop(tokens: mutable.Queue[String], acc: AbstractSyntaxTree): Either[ParsingError, AbstractSyntaxTree] = {
      ???
    }
    if (tokens.head != "(") Left(IllegalStartOfExpression(tokens.head.head))
    else {

      val initialAst:AbstractSyntaxTree = ???
      loop(tokens, initialAst)
    }
  }

  private[parsing] def getOutermostExpression(tokens: mutable.Queue[String]): mutable.Queue[String] = {
    @tailrec
    def loop(openParensCount: Int, acc: mutable.Queue[String], rest: mutable.Queue[String]): mutable.Queue[String] = {
      rest.dequeue() match {
        case "(" if openParensCount == 0 =>
          loop(openParensCount + 1, acc, rest)
        case s @ "(" =>
          loop(openParensCount + 1, acc.enqueue(s), rest)
        case ")" if openParensCount == 1 && rest.isEmpty =>
          acc
        case s @ ")" =>
          loop(openParensCount - 1, acc.enqueue(s), rest)
        case s =>
          loop(openParensCount, acc.enqueue(s), rest)
      }
    }
    loop(openParensCount = 0, mutable.Queue.empty, tokens)
  }

  def parse(tokens: mutable.Queue[String]): Either[ParsingError, AbstractSyntaxTree] = {
    @tailrec
    def loop(
              openParensCount: Int,
              buffer: List[String],
              tokens: mutable.Queue[String]
            ): Either[ParsingError, AbstractSyntaxTree] = {
      if (openParensCount == 0 && tokens.head != "(")
        Left(IllegalStartOfExpression(tokens.head.head))
      else {
        tokens.dequeue() match {
          case "(" =>
            loop(openParensCount + 1, buffer, tokens)
          case ")" if openParensCount == 0 =>
            Left(UnmatchedRightParenthesis)
          case s if !Set("(", ")").contains(s) =>
            loop(openParensCount, buffer.prepended(s), tokens)
          case ")" =>
            parseExpression(buffer)
          case s =>
            loop(openParensCount, buffer.prepended(s), tokens)
        }
      }
    }

    loop(0, List.empty, tokens)
  }

  private def parseExpression(buffer: List[String]): Either[ParsingError, AbstractSyntaxTree] = {
    buffer.indexWhere(Lexer.numericOperatorTokens.map(_.toString).contains(_)) match {
      case -1 =>
        Left(InvalidExpression)
      case i =>
        val (parsedInts, errors) = buffer
          .take(i)
          .reverse
          .map(s => Try(s.toInt))
          .partition(_.isSuccess)

        if (errors.nonEmpty) {
          errors.collect {
            case Failure(err) => Left(IntegerParseError(err.getMessage))
          }.head // TODO: Accumulate errors
        }
        else {
          val literals = parsedInts.collect {
            case Success(int) =>
              Literal(int)
          }
          NumericOperationParser.parse(buffer(i), literals)
        }
    }
  }
}

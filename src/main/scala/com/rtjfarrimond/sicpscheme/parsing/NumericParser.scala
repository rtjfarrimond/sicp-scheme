package com.rtjfarrimond.sicpscheme.parsing

import cats.data.NonEmptyList
import com.rtjfarrimond.sicpscheme.ast._
import com.rtjfarrimond.sicpscheme.parsing.ParsingError._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object NumericParser {

  def parseNumeric(tokens: NonEmptyList[String]): Either[ParsingError, Numeric] =
    tokens.toList match {
      case str :: Nil =>
        NumericParser.parseNumericLiteral(str)
      case tokensList =>
        parseNumericOperation(tokensList)
    }

  def parseNumericOperation(tokens: List[String]): Either[ParsingError, NumericOperation] = {
    val firstChar = tokens.head.head
    if (firstChar != '(')
      Left(UnknownIdentifier(tokens.head))
    else if (tokens.last.last != ')')
      Left(UnexpectedEndOfFile)
    else {
      val (exprTokens, rest) = getFirstOutermostExpressionTokens(tokens)
      if (rest.nonEmpty) Left(IllegalStartOfExpression(rest.head.head))
      else {
        parseChildren(exprTokens.tail, List.empty).flatMap { children =>
          parseNumericOperation(exprTokens.head, children)
        }
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

  @tailrec
  private def parseChildren(tokens: List[String], acc: List[Numeric]): Either[ParsingError, List[Numeric]] = {
    if (tokens.isEmpty) Right(acc)
    else tokens.head match {
      case "(" =>
        val (exprTokens, rest) = getFirstOutermostExpressionTokens(tokens)
        NumericParser.parseNumeric(NonEmptyList("(", exprTokens.appended(")"))) match {
          case Left(pe) => Left(pe)
          case Right(ast) => parseChildren(rest, acc :+ ast)
        }
      case s =>
        NumericParser.parseNumericLiteral(s) match {
          case Left(err) =>
            Left(err)
          case Right(literal) =>
            parseChildren(tokens.tail, acc :+ literal)
        }
    }
  }

  private def parseNumericOperation(
    string: String,
    children: List[Numeric]
  ): Either[UnknownIdentifier, NumericOperation] = {
    string match {
      case "+" => Right {
        if (children.nonEmpty) Plus(NonEmptyList(children.head, children.tail))
        else Plus.unit
      }
      case "*" => Right {
        if (children.nonEmpty) Multiply(NonEmptyList(children.head, children.tail))
        else Multiply.unit
      }
      case "-" if children.nonEmpty =>
        Right(Minus(NonEmptyList(children.head, children.tail)))
      case "/" if children.nonEmpty =>
        Right(Divide(NonEmptyList(children.head, children.tail)))
      case s =>
        Left(UnknownIdentifier(s))
    }
  }

  def parseNumericLiteral(string: String): Either[ParsingError, NumericLiteral] =
    Try(string.toInt) match {
      case Failure(_) =>
        Left(UnknownIdentifier(string))
      case Success(parsedInt) =>
        Right(NumericLiteral(parsedInt))
    }

}
package com.rtjfarrimond.sicpscheme

import com.rtjfarrimond.sicpscheme.ast._

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait ParsingError extends Throwable {
  def message: String
}
case class IllegalStartOfExpression(found: Char) extends ParsingError {
  override def message: String = s"Expressions must start with '(', found '$found'"
}
case class IllegalEndOfExpression(found: Char) extends ParsingError {
  override def message: String = s"Expressions must end with ')', found '$found'"
}
case class UnknownOperator(found: Char) extends ParsingError {
  override def message: String = s"Unknown operator '$found'"
}
case class IntegerParseError(s: String) extends ParsingError {
  override def message: String = s"Could not parse integer from input '$s'"
}
case class TooFewArguments(atLeast: Int, found: Int) extends ParsingError {
  override def message: String = s"too few arguments (at least: $atLeast got: $found)"
}

object Parser {
  def parse(tokens: mutable.Queue[String]): Either[ParsingError, AbstractSyntaxTree] = {
    tokens.head match {
      case "(" =>
        val nextTokens = tokens.tail.takeWhile(s => s != "(" && s != ")")
        nextTokens.head match {
          case "+" => Right(Plus.unit)
          case "-" => Left(TooFewArguments(1, 0))
          case "*" => Right(Multiply.unit)
          case "/" => Left(TooFewArguments(1, 0))
          case _ => Left(UnknownOperator(nextTokens.head.head))
        }
      case firstToken =>
        Left(IllegalStartOfExpression(firstToken.head))
    }
  }
}

//object Parser {
//  def parse(tokens: mutable.Queue[String]): mutable.Stack[Either[ParsingError, Expression]] = {
//    @tailrec
//    def loop(acc: mutable.Stack[Either[ParsingError, Expression]], rest: mutable.Queue[String]): mutable.Stack[Either[ParsingError, Expression]] = {
//      if (rest.isEmpty) acc
//      else {
//        val firstToken = rest.head.head
//        if (firstToken != ParensLeft.token) {
//          acc.push(Left(IllegalStartOfExpression(firstToken)))
//          loop(acc, rest.dropWhile(_ != ParensLeft.token.toString))
//        } else {
//          rest.dropInPlace(1) // TODO: Make this better
//          rest.indexOf(ParensRight.token.toString) match {
//            case -1 =>
//              acc.push(Left(IllegalEndOfExpression(rest.last.last)))
//            case i =>
//              val exprTokens = rest.take(i)
//              val operatorToken = exprTokens.head.head
//              NumericOperator.fromToken(operatorToken) match {
//                case None =>
//                  val err = Left(UnknownOperator(operatorToken))
//                  loop(acc.push(err), rest.dropInPlace(i + 1))
//                case Some(op) =>
//                  val postOperator = exprTokens.head.tail
//                  val exprTokensNoOperator = postOperator match {
//                    case "" => exprTokens.tail
//                    case s =>
//                      s +: exprTokens.tail
//                  }
//                  val ints = exprTokensNoOperator.map(_.toInt) // TODO: This will blow up when failing to parse int, test for it and update
//                  loop(acc.push(Right(Expression(op, ints.toSeq: _*))), rest.dropInPlace(i + 1))
//              }
//          }
//        }
//      }
//    }
//    loop(mutable.Stack.empty, tokens)
//  }
//}

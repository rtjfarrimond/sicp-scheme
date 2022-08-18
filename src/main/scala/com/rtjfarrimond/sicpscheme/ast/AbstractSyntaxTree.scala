package com.rtjfarrimond.sicpscheme.ast

import cats.data.NonEmptyList

import scala.util.{Failure, Success, Try}

sealed trait AbstractSyntaxTree {
  def value: Int
}
abstract class Node extends AbstractSyntaxTree {
  def children: NonEmptyList[AbstractSyntaxTree]
}
abstract class Leaf extends AbstractSyntaxTree

case class Literal(value: Int) extends Leaf
object Literal {
  def fromString(s: String): Option[Literal] =
    Try(s.toInt) match {
      case Failure(_) =>
        None
      case Success(parsedInt) =>
        Some(Literal(parsedInt))
    }
}

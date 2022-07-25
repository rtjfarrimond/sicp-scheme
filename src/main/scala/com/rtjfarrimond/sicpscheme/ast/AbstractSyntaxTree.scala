package com.rtjfarrimond.sicpscheme.ast

import cats.data.NonEmptyList

sealed trait AbstractSyntaxTree {
  def value: Int
}
abstract class Node extends AbstractSyntaxTree {
  def children: NonEmptyList[AbstractSyntaxTree]
}
abstract class Leaf extends AbstractSyntaxTree

case class Literal(value: Int) extends Leaf
case class Plus(children: NonEmptyList[AbstractSyntaxTree]) extends Node {
  override def value: Int = children.map(_.value).toList.sum
}
object Plus {
  def unit: Plus = Plus(List.empty)
  
  def apply(childNodes: List[AbstractSyntaxTree]): Plus =
    childNodes.match {
      case Nil =>
        Plus(NonEmptyList.of(Literal(0)))
      case head :: tail =>
        Plus(NonEmptyList(head, tail))
    }
}
case class Minus(children: NonEmptyList[AbstractSyntaxTree]) extends Node {
  override def value: Int = {
    val values = children.map(_.value)
    values.tail.foldLeft(values.head)(_ - _)
  }
}
case class Multiply(children: NonEmptyList[AbstractSyntaxTree]) extends Node {
  override def value: Int = children.map(_.value).toList.product
}
object Multiply {
  def unit: Multiply = Multiply(List.empty)
  
  def apply(childNodes: List[AbstractSyntaxTree]): Multiply =
    childNodes.match {
      case Nil =>
        Multiply(NonEmptyList.of(Literal(1)))
      case head :: tail =>
        Multiply(NonEmptyList(head, tail))
    }
}
case class Divide(children: NonEmptyList[AbstractSyntaxTree]) extends Node {
  override def value: Int = {
    val values = children.map(_.value)
    values.tail.foldLeft(values.head)(_ / _)
  }
}

// TODO: Move this to a test
object Main extends App {

  val baz = Plus(
    NonEmptyList.of(
      Plus(
        NonEmptyList.of(
          Literal(3),
          Literal(7))),
      Plus(
        NonEmptyList.of(
          Literal(2),
          Literal(30)
        )
      )
    )
  )

  println(baz.value)

}

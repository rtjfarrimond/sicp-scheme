package com.rtjfarrimond.sicpscheme.ast

import cats.data.NonEmptyList

trait NumericOperation extends Node

case class Plus(children: NonEmptyList[AbstractSyntaxTree]) extends NumericOperation {
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

case class Multiply(children: NonEmptyList[AbstractSyntaxTree]) extends NumericOperation {
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

case class Minus(children: NonEmptyList[AbstractSyntaxTree]) extends NumericOperation {
  override def value: Int = {
    val values = children.map(_.value)
    values.tail.foldLeft(values.head)(_ - _)
  }
}

case class Divide(children: NonEmptyList[AbstractSyntaxTree]) extends NumericOperation {
  override def value: Int = {
    val values = children.map(_.value)
    values.tail.foldLeft(values.head)(_ / _)
  }
}
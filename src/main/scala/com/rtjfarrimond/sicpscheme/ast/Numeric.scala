package com.rtjfarrimond.sicpscheme.ast

import cats.data.NonEmptyList
import scala.util.{Failure, Success, Try}

trait Numeric extends AbstractSyntaxTree {
  def value: Int
}

case class NumericLiteral(value: Int) extends Leaf with Numeric {
  override def stringValue: String = value.toString
}

sealed trait NumericOperation extends Node with Numeric

case class Plus(children: NonEmptyList[Numeric]) extends NumericOperation {
  override lazy val value: Int = children.map(_.value).toList.sum
  override lazy val stringValue: String = value.toString
}
object Plus {
  def unit: Plus = Plus(NonEmptyList.of(NumericLiteral(0)))

  def apply(childNodes: List[Numeric]): Plus =
    childNodes.match {
      case Nil =>
        unit
      case head :: tail =>
        Plus(NonEmptyList(head, tail))
    }
}

case class Multiply(children: NonEmptyList[Numeric]) extends NumericOperation {
  override lazy val value: Int = children.map(_.value).toList.product
  override lazy val stringValue: String = value.toString
}
object Multiply {
  def unit: Multiply = Multiply(NonEmptyList.of(NumericLiteral(1)))

  def apply(childNodes: List[Numeric]): Multiply =
    childNodes.match {
      case Nil =>
        unit
      case head :: tail =>
        Multiply(NonEmptyList(head, tail))
    }
}

case class Minus(children: NonEmptyList[Numeric]) extends NumericOperation {
  override lazy val value: Int = {
    val values = children.map(_.value)
    values.tail.foldLeft(values.head)(_ - _)
  }
  override lazy val stringValue: String = value.toString
}

case class Divide(children: NonEmptyList[Numeric]) extends NumericOperation {
  override lazy val value: Int = {
    val values = children.map(_.value)
    values.tail.foldLeft(values.head)(_ / _)
  }
  override lazy val stringValue: String = value.toString
}
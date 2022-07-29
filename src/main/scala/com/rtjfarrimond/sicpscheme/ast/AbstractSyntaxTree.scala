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

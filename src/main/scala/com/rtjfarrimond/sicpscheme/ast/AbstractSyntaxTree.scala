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

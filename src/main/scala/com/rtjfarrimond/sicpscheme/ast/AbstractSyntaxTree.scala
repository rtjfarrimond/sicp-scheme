package com.rtjfarrimond.sicpscheme.ast

import cats.data.NonEmptyList

trait AbstractSyntaxTree {
  def stringValue: String
}
abstract class Node extends AbstractSyntaxTree {
  def children: NonEmptyList[AbstractSyntaxTree]
}
abstract class Leaf extends AbstractSyntaxTree

package com.rtjfarrimond.sicpscheme

import com.rtjfarrimond.sicpscheme.ast.{AbstractSyntaxTree, Leaf, Literal, Node}

object Interpreter {
  def run(ast: AbstractSyntaxTree): Int = ast.value
}

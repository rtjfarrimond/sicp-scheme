package com.rtjfarrimond.sicpscheme.ast

// TODO: Does this really need to be an AST?
sealed trait PrimitiveProcedure extends AbstractSyntaxTree {
  override def stringValue: String = "PrimitiveProcedure"
  def symbol: Char
  def op(a: Int, b: Int): Int
}

case object PlusProc extends PrimitiveProcedure {
  override def symbol: Char = '+'
  override def op(a: Int, b: Int): Int = a + b
}
case object MinusProc extends PrimitiveProcedure {
  override def symbol: Char = '-'
  override def op(a: Int, b: Int): Int = a - b
}
case object MultiplyProc extends PrimitiveProcedure {
  override def symbol: Char = '*'
  override def op(a: Int, b: Int): Int = a * b
}
case object DivideProc extends PrimitiveProcedure {
  override def symbol: Char = '/'
  override def op(a: Int, b: Int): Int = a / b
}

object PrimitiveProcedure {
  def ofChar(c: Char): Option[PrimitiveProcedure] = c match {
    case '+' => Some(PlusProc)
    case '-' => Some(MinusProc)
    case '*' => Some(MultiplyProc)
    case '/' => Some(DivideProc)
    case _ => None
  }
}
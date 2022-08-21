package com.rtjfarrimond.sicpscheme.primitive

sealed trait PrimitiveProcedure

object PrimitiveProcedure {
  case object Plus extends PrimitiveProcedure
  case object Multiply extends PrimitiveProcedure
  case object Minus extends PrimitiveProcedure
  case object Divide extends PrimitiveProcedure

  def fromChar(c: Char): Option[PrimitiveProcedure] = c match {
    case '+' => Some(Plus)
    case '*' => Some(Multiply)
    case '-' => Some(Minus)
    case '/' => Some(Divide)
    case _ => None
  }

}

package com.rtjfarrimond.sicpscheme

sealed trait NumericOperator
sealed trait Monoidal[A] {
  def unit: A
}
case object Plus extends NumericOperator with Monoidal[Int] {
  override def unit: Int = 0
}
case object Minus extends NumericOperator
case object Multiply extends NumericOperator with Monoidal[Int] {
  override def unit: Int = 1
}
case object Divide extends NumericOperator

object NumericOperator {

  private val tokenMap: Map[Char, NumericOperator] = Map(
    '+' -> Plus,
    '-' -> Minus,
    '*' -> Multiply,
    '/' -> Divide
  )
  val tokens: Set[Char] = tokenMap.keySet

  def fromToken(token: Char): Option[NumericOperator] =
    tokenMap.get(token)

}

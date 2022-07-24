package com.rtjfarrimond

package object sicpscheme {

  sealed trait Parens {
    def token: Char
  }
  case object ParensLeft extends Parens {
    override def token: Char = '('
  }
  case object ParensRight extends Parens {
    override def token: Char = ')'
  }

  val parensTokens: Set[Char] = Set(ParensLeft.token, ParensRight.token)

}

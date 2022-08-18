package com.rtjfarrimond.sicpscheme

object Lexer {

  private val parensTokens = Set('(', ')')
  private val numericOperatorTokens = Set('+', '-', '*', '/')
  private val reservedChars = parensTokens ++ numericOperatorTokens
  private val whitespace = Set(' ', '\t')

  def tokenize(body: String): List[String] = {
    @scala.annotation.tailrec
    def loop(acc: List[String], buff: String, rest: Seq[Char]): List[String] = {
      if (rest.isEmpty) buff match {
        case "" => acc
        case _ => acc.appended(buff)
      }
      else if ((reservedChars contains rest.head) && buff.isEmpty)
        loop(acc.appended(rest.head.toString), buff, rest.tail)
      else if ((reservedChars contains rest.head) && buff.nonEmpty)
        loop(acc ++ List(buff, rest.head.toString), "", rest.tail)
      else if ((whitespace contains rest.head) && buff.isEmpty)
        loop(acc, "", rest.tail)
      else if ((whitespace contains rest.head) && buff.nonEmpty)
        loop(acc.appended(buff), "", rest.tail)
      else
        loop(acc, s"$buff${rest.head.toString}", rest.tail)
    }
    loop(List.empty, "", body)
  }

}

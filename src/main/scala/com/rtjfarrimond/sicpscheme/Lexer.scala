package com.rtjfarrimond.sicpscheme

import scala.collection.mutable

object Lexer {

  val numericOperatorTokens: Set[Char] = Set('+', '-', '*', '/') // TODO: Move this somewhere better
  private val reservedChars = parensTokens ++ numericOperatorTokens
  private val whitespace = Set(' ', '\t')

  // TODO: Make this return a list, mutability is confusing. Perhaps change back later, but for now is premature
  def tokenize(body: String): mutable.Queue[String] = {
    @scala.annotation.tailrec
    def loop(acc: mutable.Queue[String], buff: String, rest: Seq[Char]): mutable.Queue[String] = {
      if (rest.isEmpty)
        acc
      else if ((reservedChars contains rest.head) && buff.isEmpty)
        loop(acc.enqueue(rest.head.toString), buff, rest.tail)
      else if ((reservedChars contains rest.head) && buff.nonEmpty)
        loop(acc.enqueue(buff, rest.head.toString), "", rest.tail)
      else if ((whitespace contains rest.head) && buff.isEmpty)
        loop(acc, "", rest.tail)
      else if ((whitespace contains rest.head) && buff.nonEmpty)
        loop(acc.enqueue(buff), "", rest.tail)
      else
        loop(acc, s"$buff${rest.head.toString}", rest.tail)
    }
    loop(mutable.Queue.empty, "", body)
  }

}

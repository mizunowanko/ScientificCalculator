package parser

import scalaz._
import scalaz.Tree._

import ParenthesisParser._

/**
  * Created by mizuwan on 2016/09/27.
  */


object SyntaxTree{
  def apply(str: String): Tree[String] = {
    if (inAParenthesis(str))
      Node[String]("", apply(removeOuterParenthesis(str)) #:: Stream.empty)
    else if (hasParenthesis(str))
      Node[String]("", split(removeOuterParenthesis(str)).map(apply).toStream)
    else
      Leaf[String](str)
  }
}

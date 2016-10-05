package parser

import org.specs2.Specification

/**
  * Created by mizuwan on 2016/09/25.
  */
class ParenthesisParserSpec extends Specification{ def is = s2"""
      A string with no parenthesis applied to split returns a list including the string $withNoParenthesis
      A string within parenthesis applied to split returns a list including the string $withinParenthesis
      A string with left parenthesis first applied to split returns a list with multiplle strings $leftParenthesisFirst
      A string with right parenthesis last applied to split returns a list with multiplle strings $rightParenthesisLast
      A string with parenthesis in it can be applied to split $parenthesisInit

      '(3i) is inParenthesis' is true $inParenthesis
      '(3i)+1 is inParenthesis' is false $notInParenthesis
      '(3i)+(1) is inParenthesis' is false $notInParenthesis2

      removeOuterParenthesis can be used $testRemoveOuterParenthesis
    """

  def withNoParenthesis = {ParenthesisParser.split("3i") must_== List("3i")}
  def withinParenthesis = {ParenthesisParser.split("(3i)") must_== List("(3i)")}
  def leftParenthesisFirst = {ParenthesisParser.split("(3i)+2") must_== List("(3i)", "+2")}
  def rightParenthesisLast = {ParenthesisParser.split("3+(2)") must_== List("3+", "(2)")}
  def parenthesisInit = {ParenthesisParser.split("2+(3)+1") must_== List("2+", "(3)", "+1")}

  def inParenthesis = {ParenthesisParser.inParenthesis("(3i)") must_== true}
  def notInParenthesis = {ParenthesisParser.inParenthesis("(3i)+1") must_== false}
  def notInParenthesis2 = {ParenthesisParser.inParenthesis("(3i)+(1)") must_== false}

  def testRemoveOuterParenthesis = {ParenthesisParser.removeOuterParenthesis("(a)") must_== "a"}
  def testRemoveOuterParenthesis2 = {ParenthesisParser.removeOuterParenthesis("(a)+(2)") must_== "(a)+(2)"}

  def testHasParenthesis = {ParenthesisParser.hasParenthesis("a") must_== false}
  def testHasParenthesis2 = {ParenthesisParser.hasParenthesis("(a)") must_== true}

  def testInAParenthesis1 = {ParenthesisParser.inAParenthesis("a") must_== false}
  def testInAParenthesis2 = {ParenthesisParser.inAParenthesis("(a)") must_== true}
  def testInAParenthesis3 = {ParenthesisParser.inAParenthesis("((a))") must_== false}
}

package expression

import org.specs2.Specification
import value.Complex

/**
  * Created by mizuwan on 2016/09/11.
  */
class ExpressionTreeSpec extends Specification{ def is = s2"""
  A real number expression should be evaluated as a real number           $evaluateRealNumber
  A pure imaginary number expression should be evaluated as a pure number $evaluatePureNumber
  I should be evaluated as I                                              $evaluateI
  A term including + should be evaluated as addition                      $evaluateAdd
  """

  def evaluateRealNumber  = ExpressionTree("2").evaluate    must_== Complex(2, 0)
  def evaluatePureNumber  = {ExpressionTree("2i").evaluate  must_== Complex(0, 2)}
  def evaluateI           = {ExpressionTree("i").evaluate   must_== Complex(0, 1)}
  def evaluateAdd         = {ExpressionTree("1+3").evaluate must_== Complex(4, 0)}
}

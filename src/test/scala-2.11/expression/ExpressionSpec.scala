package expression

import org.specs2.Specification
import value.Complex

/**
  * Created by mizuwan on 2016/09/11.
  */
class ExpressionSpec extends Specification{ def is = s2"""
  A real number expression should be evaluated as a real number           $evaluateRealNumber
  A pure imaginary number expression should be evaluated as a pure number $evaluatePureNumber

  """

  def evaluateRealNumber  = {Expression("2").evaluate   must_== Complex(2, 0)}
  def evaluatePureNumber  = {Expression("2i").evaluate  must_== Complex(0, 2)}
  def evaluateI           = {Expression("i").evaluate   must_== Complex(0, 1)}
}

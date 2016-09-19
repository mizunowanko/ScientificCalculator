package expression

import value._

/**
  * Created by mizuwan on 2016/09/11.
  */

//expression tree
trait ExpressionTree[+T <: Value]{

  //the expression String
  val str: String

  //the type of the evaluator
  type EvaluatorType

  //the function to evaluate itself
  val evaluator: EvaluatorType

  //return its value
  def evaluate: T
}

//
object ExpressionTree{

  //inner class that indicates an expression
  //it is used to create expression tree
  case class Expression[T](_str: String){
    val str = _str
  }

  //define all regex types
  val re  = """^(-?\d(\.\d)*)$""".r
  val im  = """^(-?\d(\.\d*)?)i$""".r
  val i   = """^i$""".r
  val addComplex = """^((-?\d(\.\d)*)|(-?\d(\.\d*)?)i)(\+)(.*)$""".r

  //expression factory
  def apply(str: String): ExpressionTree[Value] = {

    val expression = str match {
      case re(_, _) => Expression[Complex](str)
      case im(_, _) => Expression[Complex](str)
      case i()      => Expression[Complex](str)
      case addComplex(_, _, _, _, _, _, _) => Expression[Complex](str)
      case _ => sys.error("Assigned string does not match any expression type of expression")
    }

    apply(expression)

  }

  def apply[T <: Value](expression: Expression[T])(implicit expressionTreeFactory: Expression[T] => ExpressionTree[T]): ExpressionTree[T] = {
    expressionTreeFactory(expression)
  }

  implicit def complexExpressionTreeFactory(expression: Expression[Complex]): ExpressionTree[Complex] = {
    import Operand._
    import BinaryOperator._
    val str = expression.str
    str match {
      case re(_, _) => Operand[Complex](str)
      case im(_, _) => Operand[Complex](str)
      case i() => Operand[Complex](str)
      case addComplex(z1, _, _, _, _, plus, z2) => {
        val left: ExpressionTree[Complex] = ExpressionTree[Complex](Expression(z1))
        val right: ExpressionTree[Complex] = ExpressionTree[Complex](Expression(z2))
        BinaryOperator[Complex](plus)(left)(right)
      }
    }
  }
}


//the expression that indicates operand
//It's expressed as Function0
case class Operand[T <: Value](_str: String)(implicit getEvaluator: String => (() => T)) extends ExpressionTree[T]{

  //the type of evaluator
  override type EvaluatorType = () => T

  //string expression
  val str: String = _str

  //returns complex
  override val evaluator = getEvaluator(str)

  //returns value
  override def evaluate = evaluator()
}

object Operand{
  implicit def getComplexOperandEvaluator(str: String): () => Complex =
    () => Complex(str)
}



/*
//the expression that indicates unary operator
//It's expressed as Function1
case class UnaryOperator(_str: String) extends ExpressionTree{

  //the type of evaluator
  override type EvaluatorType = Value => Value

  //string expression
  val str: String = _str

  //returns complex
  override val evaluator: EvaluatorType = {() => Value(str)}

  //returns value
  override def evaluate: Value = evaluator()
}

object UnaryOperator{

  //map String to Function
  val functionMapper = {
    "+":
  }
}
*/

//the expression that indicates binary operator
//It's expressed as Function2

import BinaryOperator._

case class BinaryOperator[T <: Value]
  (_str: String)
  (_left: ExpressionTree[T])
  (_right: ExpressionTree[T])
  (implicit getEvaluator: String => T => T => T) extends ExpressionTree[T]{

  //string expression
  val str: String = _str

  //children of the tree
  val left: ExpressionTree[T] = _left
  val right: ExpressionTree[T] = _right

  //the type of evaluator
  override type EvaluatorType = T => T => T

  //returns complex
  override val evaluator: EvaluatorType = getEvaluator(str)

  //returns value
  override def evaluate: T = evaluator(left.evaluate)(right.evaluate)
}

object BinaryOperator{

  //map String to Function
  implicit def getComplexBinaryOperatorEvaluator(str: String): Complex => Complex => Complex = {
    val addRegex = """^\+$""".r

    str match {
      case addRegex() => Complex.add
      case _ => sys.error("Assigned string does not match any expression type of Complex BinaryOperator")
    }
  }



}

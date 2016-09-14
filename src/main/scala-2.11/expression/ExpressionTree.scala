package expression

import value.Value

/**
  * Created by mizuwan on 2016/09/11.
  */

//expression tree
trait ExpressionTree{

  //the expression String
  val str: String

  //the type of the evaluator
  type EvaluatorType

  //the function to evaluate itself
  val evaluator: EvaluatorType

  //return its value
  def evaluate: Value
}

//
object ExpressionTree{

  //expression factory
  def apply(str: String): ExpressionTree = {

    //define all regex types
    val re  = """^(-?\d(\.\d)*)$""".r
    val im  = """^(-?\d(\.\d*)?)i$""".r
    val i   = """^i$""".r

    //specify the expression type by the regex
    str match {
      case re(_, _) => Operand(str)
      case im(_, _) => Operand(str)
      case i()      => Operand(str)
      case _ => sys.error("Assigned string does not match any expression type")
    }
  }
}


//the expression that is operated by operators
//It's expressed as Function0
case class Operand(_str: String) extends ExpressionTree{

  //the type of evaluator
  override type EvaluatorType = ()=> Value

  //string expression
  val str: String = _str

  //returns complex
  override val evaluator: EvaluatorType = {() => Value(str)}

  //returns value
  override def evaluate: Value = evaluator()
}
package expression

import value.Value

/**
  * Created by mizuwan on 2016/09/11.
  */

//
trait Expression{

  //expression String
  val str: String

  //return value
  def evaluate: Value
}

object Expression {

  //expression factory
  def apply(str: String): Expression = {

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
/*
case class Expandable(_str: String) extends Expression{

  val str: String = _str

  def evaluate: Value

  //private def expand: UnExpandable
}
*/
//the component that consists the expression like operators and operands
trait UnExpandable extends Expression{

  //the type of evaluator
  type FunctionN

  //string expression
  val str: String

  //the function this.evaluate uses
  val evaluator: FunctionN

  //return its value
  def evaluate: Value
}

//the expression that is operated by operators
//It's expressed as Function0
case class Operand(_str: String) extends UnExpandable{

  //the type of evaluator
  override type FunctionN = ()=> Value

  //string expression
  val str: String = _str

  //returns complex
  override val evaluator: FunctionN = {() => Value(str)}

  //returns value
  override def evaluate: Value = evaluator()
}
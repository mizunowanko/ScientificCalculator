/**
  * Created by mizuwan on 2016/09/10.
  */

import scala.language.implicitConversions

//class intdicates complex
case class Complex(_re: Double, _im: Double){

  //real part
  val re: Double = _re

  //imaginary part
  val im: Double = _im

  //override toString
  override def toString: String = {
    this match {
      case Complex(`re`, 0) => `re`
      case Complex(0, `im`) => imToString(`im`)
      case _              => doubleToString(`re`) + "+" + imToString(`im`)
    }
  }

  //Indicates im as String
  //If im is 1, only shows 'i'
  private def imToString(im: Double): String = {
    im match {
      case 1 => "i"
      case _ => doubleToString(im) + "i"  //why implicit conversion doesn't work?
    }
  }

  //Implicitly converts Double to String
  //If it's equivalent to Int, it shows as if Int
  implicit def doubleToString(d: Double): String = {
    val equivalentToInt = """(-)?(\d+)(\.0)""".r
    equivalentToInt findFirstIn d.toString match {
      case Some(str) => (str split """\.""")(0)
      case None => d.toString
    }
  }

}



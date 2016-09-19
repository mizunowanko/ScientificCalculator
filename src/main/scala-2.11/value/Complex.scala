package value

/**
  * Created by mizuwan on 2016/09/10.
  */

import scala.language.implicitConversions

//class that indicates complex
case class Complex(_re: Double, _im: Double) extends Value{

  //real part
  val re: Double = _re

  //imaginary part
  val im: Double = _im

  //override toString
  override def toString: String = {
    this match {
      case Complex(`re`, 0)                 => `re`
      case Complex(0, `im`)                 => imToString(`im`)
      case Complex(`re`, `im`) if `im` > 0  => doubleToString(`re`) + "+" + imToString(`im`)
      case Complex(`re`, `im`) if `im` < 0  => doubleToString(`re`) + "-" + imToString(`im`.abs)
    }
  }

  //Indicates im as String
  //If im is 1, only shows 'i'
  private def imToString(im: Double): String = {
    im match {
      case 1  => "i"
      case -1 => "-i"
      case _  => doubleToString(im) + "i"  //why implicit conversion doesn't work?
    }
  }

  //Implicitly converts Double to String
  //If Double value is equivalent to Int, it shows as if Int
  implicit def doubleToString(d: Double): String = {
    val equivalentToInt = """(-)?(\d+)(\.0)""".r
    equivalentToInt findFirstIn d.toString match {
      case Some(str)  => (str split """\.""")(0)
      case None       => d.toString
    }
  }


}

object Complex{

  def apply(str: String): Complex = Complex(complexStrToTuple(str)._1, complexStrToTuple(str)._2)

  def add(left: Complex)(right: Complex)(implicit abelianComplex: AbelianGroup[Complex]): Complex = abelianComplex.add(left)(right)

  //convert a string expressing complex to tuple (re, im)
  private def complexStrToTuple(str: String): (Double, Double) = {

    //define all patterns of regular expressions
    val re        = """^(-?\d(\.\d)*)$""".r
    val im        = """^(-?\d(\.\d*)?)i$""".r
    val i         = """^i$""".r
    val minusI    = """^-i$""".r
    val rePlusI   = """^(-?\d(\.\d)*)\+i$""".r
    val reMinusI  = """^(-?\d(\.\d)*)-i$""".r
    val rePlusIm  = """^(-?\d(\.\d)*)\+(\d(\.\d*)?)i$""".r
    val reMinusIm = """^(-?\d(\.\d)*)-(\d(\.\d*)?)i$""".r

    //pattern match by regex
    str match {
      case re(x, _)               => (x.toDouble, 0.0)
      case im(y, _)               => (0.0, y.toDouble)
      case i()                    => (0.0, 1.0)
      case minusI()               => (0.0, -1.0)
      case rePlusI(x, _)          => (x.toDouble, 1.0)
      case reMinusI(x, _)         => (x.toDouble, -1.0)
      case rePlusIm(x, _, y, _)   => (x.toDouble, y.toDouble)
      case reMinusIm(x, _, y, _)  => (x.toDouble, -(y.toDouble))
      case _                      => sys.error("the string does not match any complex expression")
    }
  }
  /*
  //add two complexes


  def subtract(left: Complex, right: Complex): Complex = {
    val Complex(a, b) = left
    val Complex(c, d) = right
    return Complex(a - c, b - d)
  }

  def multiply(left: Complex, right: Complex): Complex = {
    val Complex(a, b) = left
    val Complex(c, d) = right
    return Complex(a * c - b * d, a * d + b * c)
  }

  def divide(left: Complex, right: Complex): Complex = {
    val Complex(a, b) = left
    val Complex(c, d) = right
    return Complex((a * c + b * d)/(a * a + b * b), (a * d - b * c)/(a * a + b * b))
  }
  */
}
package value

/**
  * Created by mizuwan on 2016/09/11.
  */


// a trait that indicates some mathematical value
trait Value

// a trait that can be added, subtracted, and negated
trait AbelianGroup[T <: Value]{
  def add(t1: T)(t2: T): T
  def subtract(t1: T)(t2: T): T
  def negate(t: T): T
}

object AbelianGroup{
  implicit object ComplexAberian extends AbelianGroup[Complex]{
    override def add(z1: Complex)(z2: Complex): Complex = {
      val Complex(x1, y1) = z1
      val Complex(x2, y2) = z2
      Complex(x1 + x2, y1 + y2)
    }

    override def subtract(z1: Complex)(z2: Complex): Complex = {
      val Complex(x1, y1) = z1
      val Complex(x2, y2) = z2
      Complex(x1 - x2, y1 - y2)
    }

    override def negate(z: Complex): Complex = {
      val Complex(x, y) = z
      Complex(-x, -y)
    }
  }
}


/*
//the factory that create values
trait ValueFactory[T]{
  def factory(str: String): T
}

object ValueFactory{
  implicit object ComplexFactory extends ValueFactory[Complex]{
    def factory(str: String): Complex = {
      new Complex(str)
    }
  }
}



//supply functions that interprete strings to functions
trait OperatorInterpreter[T<:Value]{
  def strToBinaryOperatorFunction(str: String): (T, T) => T
}

object OperatorInterpreter{
  implicit object ComplexOperatorInterpreter extends OperatorInterpreter[Complex]{
    override def strToBinaryOperatorFunction(str: String): (Complex, Complex) => Complex = {
      str match{
        case "+" => Complex.add
        case "-" => Complex.subtract
        case "*" => Complex.multiply
        case "/" => Complex.divide
        case _  => sys.error("The string can't be interpreted as a binary operator.")
      }
    }
  }
}

trait FourArithmeticOperations{

  def add(left: Value, right: Value): Value
  def subtract(left: Value, right: Value): Value
  def multiply(left: Value, right: Value): Value
  def divide(left: Value, right: Value): Value
}
*/
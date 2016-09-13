package value

/**
  * Created by mizuwan on 2016/09/11.
  */

// a trait that indicates some mathematical value
trait Value {

}

object Value{
  def apply(str: String) = {

    //just create Complex at this point
    //If this application deals with the other values like vector, matrix or tensor,
    //this method will be extended.
    new Complex(str)
  }
}
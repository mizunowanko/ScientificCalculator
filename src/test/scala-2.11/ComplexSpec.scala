import org.specs2.Specification

/**
  * Created by mizuwan on 2016/09/10.
  */
class ComplexSpec extends Specification{ def is = s2"""
  toString should return 0 if re and im is 0              $show0

  toString should return only re if im is 0               $showOnlyRe

  toString should return only im if re is 0               $showOnlyIm

  toString should return i if re is 0 and im is 1         $showI

  toString should return re + i if re is not 0 and i = 0  $showReAndI

  toString should return re + im if neither is 0          $showReAndIm
  """

  def show0 = {
    val zero = Complex(0, 0)
    zero.toString must_== "0"
  }

  def showOnlyRe = {
    val realNumber = Complex(1, 0)
    realNumber.toString must_== "1"
  }

  def showOnlyIm = {
    val pureImaginaryNumber = Complex(0, 2)
    pureImaginaryNumber.toString must_== "2i"
  }

  def showI = {
    val i = Complex(0, 1)
    i.toString must_== "i"
  }

  def showReAndI = {
    val complexWithI = Complex(1, 1)
    complexWithI.toString must_== "1+i"
  }

  def showReAndIm = {
    val complex = Complex(1, 2)
    complex.toString must_== "1+2i"
  }
}

package value

import org.specs2.Specification

/**
  * Created by mizuwan on 2016/09/10.
  */
class ComplexSpec extends Specification{ def is = s2"""

  //test toString
  tensor.Complex(0, 0)   should show '0'     $show0
  tensor.Complex(x, 0)   should show 'x'     $showOnlyRe
  tensor.Complex(0, y)   should show 'yi'    $showOnlyIm
  tensor.Complex(0, 1)   should show 'i'     $showI
  tensor.Complex(0, -1)  should show '-i'    $showMinusI
  tensor.Complex(x, 1)   should show 'x+i'   $showRePlusI
  tensor.Complex(x, -1)  should show 'x-i'   $showReMinusI
  tensor.Complex(x, y>0) should show 'x+yi'  $showRePlusIm
  tensor.Complex(x, y<0) should shouw 'x-yi' $showReMinusIm

  //test sub constructor
  'x'             should construct tensor.Complex(x, 0)    $x
  'yi'            should construct tensor.Complex(0, y)    $yi
  'i'             should construct tensor.Complex(0, 1)    $i
  '-i'            should construct tensor.Complex(0, -1)   $minusI
  'x+i'           should construct tensor.Complex(x, i)    $xPlusI
  'x-i'           should construct tensor.Complex(x, -1)   $xMinusI
  'x+yi'          should construct tensor.Complex(x, y)    $xPlusYi
  'x-yi'          should construct tensor.Complex(x, -y)   $xMinusYi
  invalid string  should raise a runtime Exception  $constructError

  """

  //test toString
  def show0         = Complex(0, 0).toString    must_== "0"
  def showOnlyRe    = Complex(1, 0).toString    must_== "1"
  def showOnlyIm    = Complex(0, 2).toString    must_== "2i"
  def showI         = Complex(0, 1).toString    must_== "i"
  def showMinusI    = Complex(0, -1).toString   must_== "-i"
  def showRePlusI   = Complex(1, 1).toString    must_== "1+i"
  def showReMinusI  = Complex(1, -1).toString   must_== "1-i"
  def showRePlusIm  = Complex(1, 2).toString    must_== "1+2i"
  def showReMinusIm = Complex(1, -2).toString   must_== "1-2i"

  //test sub constructor
  def x               = new Complex("2")    must_== Complex(2, 0)
  def yi              = new Complex("2i")   must_== Complex(0, 2)
  def i               = new Complex("i")    must_== Complex(0, 1)
  def minusI          = new Complex("-i")   must_== Complex(0, -1)
  def xPlusI          = new Complex("2+i")  must_== Complex(2, 1)
  def xMinusI         = new Complex("2-i")  must_== Complex(2, -1)
  def xPlusYi         = new Complex("1+2i") must_== Complex(1, 2)
  def xMinusYi        = new Complex("1-2i") must_== Complex(1, -2)
  def constructError  = new Complex("1++i") must throwA[RuntimeException]
}

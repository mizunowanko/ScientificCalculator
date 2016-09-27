package helper

import org.specs2.Specification

import helper.ListHelper._

/**
  * Created by mizuwan on 2016/09/26.
  */
class ListHelperSpec extends Specification{ def is = s2"""
    neighbor can be used $neighborReturnsDifferences
    appendByDList can be used  $testAppendByDList
    """

  def neighborReturnsDifferences = {
    def subtract(a: Int, b: Int): Int = a - b
    neighbor(List(4, 3, 2, 3))(subtract) must_== List(1, 1, -1)
  }
  def testAppendByDList = {
    appendByDList(List(1, 2))(3) must_== List(1, 2, 3)
  }
}

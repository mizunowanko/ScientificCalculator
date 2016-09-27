package helper

import scalaz._

/**
  * Created by mizuwan on 2016/09/26.
  */

object ListHelper{

  /** returns list consisting of values calculated by adjacent elements */
  def neighbor[A, B](list: List[A])(f: (A, A) => B): List[B] = {
    Range(1, list.length).toList.map(i => f(list(i - 1), list(i)))
  }

  /** append element using DList*/
  def appendByDList[A](list: List[A])(elem: A): List[A] = {
    (DList.fromList(list) :+ elem).toList
  }
}

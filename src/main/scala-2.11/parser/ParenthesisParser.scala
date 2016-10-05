package parser

import helper.ListHelper._

/**
  * Created by mizuwan on 2016/09/25.
  */
class ParenthesisParser {
}

/** Parse string from the viewpoint of parentheses */
object ParenthesisParser{
  def split(str: String): List[String] = {
    val changedBetweenZeroAndOneList = getChangedBetweenZeroAndOneList(getDepthList(str))
    val changedIndexes = changedBetweenZeroAndOneList.zipWithIndex.filter(x => x._1 == true).map(x => x._2)
    splitByIndeces(str)(changedIndexes)
  }

  def inParenthesis(str: String): Boolean = {
    val depthList = getDepthList(str)
    val endsWithOne = depthList.last == 1
    val noZero = depthList.filter(x => x == 0) == Nil
    val startsAndEndsWithParenthesis = ("""^\(.*\)$""".r findFirstIn str).isDefined
    endsWithOne && noZero && startsAndEndsWithParenthesis
  }

  def inAParenthesis(str: String): Boolean = {
    val depthList = getDepthList(str)
    val allOne = depthList.filter(x => x == 1).length == depthList.length
    val startsAndEndsWithParenthesis = ("""^\(.*\)$""".r findFirstIn str).isDefined
    allOne && startsAndEndsWithParenthesis
  }

  def removeOuterParenthesis(str: String): String = {
    if (inParenthesis(str)){
      val withinParenthesis = """^\((.*)\)$""".r
      str match {
        case withinParenthesis(x) => x
      }
    }else{
      str
    }
  }

  def hasParenthesis(str: String): Boolean ={
    ("""(\(|\))""".r findFirstIn str).isDefined
  }

  /** convert string to list of depth calculated by the parentheses */
  private def getDepthList(str: String): List[Int] = {
    val charList = str.toList
    charList.scanLeft(('_', 0))(getDepth).map(x => x._2).tail
  }

  /** calculate the depth of the char by the previous char and depth */
  private def getDepth(prev: (Char, Int), nowChar: Char): (Char, Int) = {
    val depth: Int = (prev._1, nowChar) match {
      case (')', '(') => sys.error("A left parenthesis cannot be followed by a right parenthesis")
      case (')', _) => prev._2 - 1
      case (_, '(') => prev._2 + 1
      case _ => prev._2
    }
    (nowChar, depth)
  }


  private def getChangedBetweenZeroAndOneList(depthList: List[Int]): List[Boolean] = {
    neighbor((0 :: depthList))(changedBetweenZeroAndOne)
  }

  /** returns if the depth is changed 0 to 1 or 1 to 0 */
  private def changedBetweenZeroAndOne(prev: Int, now: Int): Boolean = {
    (prev, now) match {
      case (1, 0) => true
      case (0, 1) => true
      case _  => false
    }
  }

  /** convert  */
  private def splitByIndeces(str: String)(indexes: List[Int]): List[String] = {
    indexes match {
      case Nil => List(str)
      case 0 :: _ => neighbor(appendByDList(indexes)(str.length))(str.slice)
      case _ =>  neighbor(appendByDList(0 :: indexes)(str.length))(str.slice)
    }
  }

}


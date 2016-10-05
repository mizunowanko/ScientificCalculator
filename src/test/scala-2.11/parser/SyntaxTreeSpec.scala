package parser

import org.specs2.Specification
import scalaz._
import scalaz.Tree._

/**
  * Created by mizuwan on 2016/09/27.
  */
class SyntaxTreeSpec extends Specification{ def is = s2"""
  Laef can be created                      $onlyLeaf
  Tree can be created                      $createComplecatedTree
  """

  def onlyLeaf = {
    SyntaxTree("3").drawTree must_== Leaf[String]("3").drawTree
  }


  def createComplecatedTree = {
    SyntaxTree("4-(2*(1+2))+3").drawTree must_==
    Node[String]("",
      Leaf[String]("4-")
      #:: Node[String]("",
        Leaf[String]("2*")
        #:: Node[String]("", Leaf[String]("1+2") #:: Stream.empty) #:: Stream.empty
      )
      #:: Leaf[String]("+3") #:: Stream.empty
    ).drawTree
  }

  implicit val showString = new Show[String]{
    override def show(s: String) = s
  }
}
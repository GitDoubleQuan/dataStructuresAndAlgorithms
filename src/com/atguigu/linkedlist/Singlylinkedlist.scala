package com.atguigu.linkedlist

import scala.collection.mutable

object TestSinglylinkedlist {
  def main(args: Array[String]): Unit = {
    val singlylinkedlist = new Singlylinkedlist
    singlylinkedlist.init(10)
    singlylinkedlist.show
    println("--------------")
    singlylinkedlist.reversal
    singlylinkedlist.show
    println("--------------")
    singlylinkedlist.reversedShow
  }
}

class Singlylinkedlist {

  var head: SNode = new SNode(-1, null)

  def init(size: Int): Unit = {
    var tmpNode = head
    for (no <- 1 to size) {
      val newNode = new SNode(no, null)
      tmpNode.next = newNode
      tmpNode = newNode
    }
  }

  def show: Unit ={
    var tmpNode = head
    while (tmpNode.next != null){
      println(tmpNode.next.no)
      tmpNode = tmpNode.next
    }
  }

  def reversal: Unit ={
    var tmpA = head.next
    if(tmpA == null){
      println("链表为空")
      return
    }
    var tmpB = tmpA.next
    if(tmpB == null){
      return
    }
    var tmpC = tmpB.next
    tmpA.next = null
    tmpB.next = tmpA
    while (tmpC != null){
      tmpA = tmpB
      tmpB = tmpC
      tmpC = tmpC.next
      tmpB.next = tmpA
    }
    head.next = tmpB
  }

  def reversedShow: Unit ={
    var tmp = head.next
    if(tmp == null){
      return
    }
    val nodes = new mutable.Stack[SNode]
    while (tmp != null){
      nodes.push(tmp)
      tmp = tmp.next
    }
    nodes.map(_.no).foreach(println(_))
  }

}

class SNode(val no: Int, var next: SNode)

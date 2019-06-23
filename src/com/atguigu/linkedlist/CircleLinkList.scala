package com.atguigu.linkedlist

import util.control.Breaks._

object test {
  def main(args: Array[String]): Unit = {
    val list = new CircleLinkList
//    list.init(5)
//    list.show()

    list.josephGame(6, 2, 3)
  }
}

class CircleLinkList {
  var firstNode = new Node(-1)
  var currentNode: Node = _

  def init(count: Int): Unit = {
    if (count < 1) {
      return
    }
    for (no <- 1 to count) {
      val newNode = new Node(no)
      if (firstNode.next == null) {
        firstNode = newNode
        newNode.next = newNode
      } else {
        currentNode.next = newNode
        newNode.next = firstNode
      }
      currentNode = newNode
    }
  }

  def show(): Unit = {
    if (firstNode.next == null) {
      return
    }
    var tmpNode = firstNode
    breakable {
      while (true) {
        println(s"node no : ${tmpNode.no}")
        if (tmpNode.next == firstNode) {
          break()
        }
        tmpNode = tmpNode.next
      }
    }
  }

  def josephGame(size: Int, startNo: Int, interval: Int): Unit = {
    if (size < 1 || startNo > size || interval < 1) {
      println("参数错误")
      return
    }

    //初始化链表
    init(size)

    //初始化头指针
    var tmp = firstNode
    var head: Node = null
    breakable {
      while (true) {
        if (tmp.next.no == startNo) {
          head = tmp
          break()
        }
        tmp = tmp.next
      }
    }

    //叫数
    while (head.next != head) {
      for (i <- 1 until interval){
        head = head.next
      }
      println(head.next.no)
      head.next = head.next.next
    }
    println(head.no)
  }

}

class Node(val no: Int) {
  var next: Node = _
}

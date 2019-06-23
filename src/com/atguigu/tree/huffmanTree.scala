package com.atguigu.tree

import java.util
import java.util.Collections

object huffmanTree {

  def main(args: Array[String]): Unit = {
    val arr = Array[Int](13, 7, 8, 3, 29, 6, 1)
    val node = createHuffmanTree(arr)
    perOrder(node)
  }

  /**
    * 创建霍夫曼树
    *
    * @param arr 用于创建霍夫曼树的数组
    * @return
    */
  def createHuffmanTree(arr: Array[Int]): HuffmanNode = {

    //先给传入的数组排序
    val arrList = new util.ArrayList[HuffmanNode]()
    arr.foreach(e => arrList.add(new HuffmanNode(e)))
    Collections.sort(arrList)

    var leftNode: HuffmanNode = null
    var rightNode: HuffmanNode = null
    var parentNode: HuffmanNode = null
    //因为每次都从list中删除两个，增加一个元素，
    // 当list中只有一个元素的时候说明霍夫曼树创建完毕
    while (arrList.size() > 1) {
      //从从小到大拍好序的list中，取出最小的两个节点
      leftNode = arrList.get(0)
      rightNode = arrList.get(1)
      //用这两个节点创建一个新的节点，新节点的权重是这两个节点的和
      parentNode = new HuffmanNode(leftNode.value + rightNode.value)
      //并且这两个节点作为新节点的子节点
      parentNode.left = leftNode
      parentNode.right = rightNode
      //往list中添加新创建的节点并删除，和两个旧节点
      arrList.remove(leftNode)
      arrList.remove(rightNode)
      arrList.add(parentNode)
      //对list重新排序，重复上面的过程：取出最小的两个节点，组成一颗新树，在把新树的root节点放回list中
      Collections.sort(arrList)
    }
    arrList.get(0)
  }

  def perOrder(node: HuffmanNode): Unit = {
    node.preOrder
  }

}

/**
  * 节点，混入Comparable特质方便排序
  *
  * @param value
  */
class HuffmanNode(val value: Int) extends Comparable[HuffmanNode] {
  var left: HuffmanNode = _
  var right: HuffmanNode = _


  /**
    * 前序遍历
    */
  def preOrder: Unit = {
    println(this)
    if (left != null) {
      left.preOrder
    }
    if (right != null) {
      right.preOrder
    }
  }

  override def compareTo(o: HuffmanNode): Int = {
    this.value - o.value
  }

  override def toString: String = {
    s"{value:$value}"
  }

}

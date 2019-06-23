package com.atguigu.tree

object TestBinarySortTree {
  def main(args: Array[String]): Unit = {
    val arr = Array[Int](7, 3, 10, 12, 5, 1, 9)
    val tree = new BinarySortTree
    arr.foreach(e => tree.addNode(new BinarySortNode(e)))
    tree.middleOrder
  }
}

/**
  * 定义二叉排序树
  */
class BinarySortTree {
  var root: BinarySortNode = _

  def middleOrder: Unit = {
    if (root != null) root.middleOrder else println("二叉排序树为空")
  }

  /**
    * 给排序二叉树添加节点
    *
    * @param node
    */
  def addNode(node: BinarySortNode): Unit = {
    if (root == null) root = node else root.addNode(node)
  }
}

/**
  * 定义二叉排序树节点
  *
  * @param value 节点的值
  */
class BinarySortNode(val value: Int) {
  var left: BinarySortNode = _
  var right: BinarySortNode = _

  /**
    * 查找值为value的父节点
    * @param value
    * @return
    */


  /**
    * 查找值为value的节点
    *
    * @param value
    * @return
    */
  def searchNode(value: Int): BinarySortNode = {

    //如果当前节点的值等于value，则返回当前节点
    if (this.value == value) {
      return this
    }
    //如果value小于当前节点的值，并且当前节点的左子节点不为空，向左递归查找
    if (this.value > value && left != null) {
      left.searchNode(value)
      //如果value大于当前节点的值，并且当前节点的右子节点不为空，向右递归查找
    } else if (this.value <= value && right != null) {
      right.searchNode(value)
    } else {//除此之外的情况说明没找到
      null
    }
  }

  /**
    * 中序遍历
    */
  def middleOrder: Unit = {
    if (left != null) {
      left.middleOrder
    }
    println(this.value)
    if (right != null) {
      right.middleOrder
    }
  }

  /**
    * 给当前节点添加子节点
    *
    * @param node 待添加节点
    */
  def addNode(node: BinarySortNode): Unit = {
    //如果待添加节点的值小于当前节点的值
    if (node.value < this.value) {
      //如果当前节点的左叶子节点为空则直接放在左叶子节点上
      if (this.left == null) {
        this.left = node
      } else {
        //如果做叶子节点不为空则继续递归添加
        this.left.addNode(node)
      }
    } else {
      if (this.right == null) {
        this.right = node
      } else {
        this.right.addNode(node)
      }
    }
  }
}

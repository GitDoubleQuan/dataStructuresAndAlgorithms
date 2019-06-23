package com.atguigu.tree

object TestACLTree {
  def main(args: Array[String]): Unit = {
    val arr = Array[Int](3, 4, 5, 6, 7, 8)
    val tree = new ACLTree
    arr.foreach(e => tree.addNode(new AVLNode(e)))
    tree.middleOrder


    println("----------")

    println(tree.root.hight)

    println(tree.root.leftHight)

    println(tree.root.rightHight)
  }
}

class ACLTree {
  var root: AVLNode = _

  /**
    * 删除指定节点
    *
    * @param value
    */
  def delNode(value: Int): Unit = {
    if (root == null) return
    val targetNode = root.searchNode(value)
    if (targetNode == null) return
    //如果当前二叉树只有一个节点
    if (root.left == null && root.right == null) {
      root = null
      return
    }
    val parentNode = root.searchParentNode(value)
    //情况一：如果要删除的节点是叶子节点
    if (targetNode.left == null && targetNode.right == null) {
      //判断要删除的节点是否是父节点的左叶子节点
      if (parentNode.left != null && parentNode.left.value == value) {
        parentNode.left = null
      }
      //判断要删除的节点是否是父节点的右叶子节点
      if (parentNode.right != null && parentNode.right.value == value) {
        parentNode.right = null
      }
      //情况三：删除有两颗子树的节点
    } else if (targetNode.left != null && targetNode.right != null) {
      //找到要删除的目标节点的右子树的最小节点
      val minVal = delMinNode(targetNode.right)
      targetNode.value = minVal
      //情况二：删除只有一颗子树的节点
    } else {
      //1.如果要删除节点有的是左子节点
      if (targetNode.left != null) {
        if (parentNode != null) {
          //1.1并且待删除节点是其父节点的左子节点
          if (parentNode.left.value == value) {
            parentNode.left = targetNode.left
            //1.2并且待删除节点的是其父节点的右子节点
          } else {
            parentNode.right = targetNode.left
          }
        } else {
          root = targetNode.left
        }
        //2.如果待删除节点有的是右子节点
      } else {
        if (parentNode != null) {
          //2.1并且待删除节点是其父节点的左子节点
          if (parentNode.left.value == value) {
            parentNode.left = targetNode.right
          } else {
            //2.2并且待删除节点的是其父节点的右子节点
            parentNode.right = targetNode.right
          }
        } else {
          root = targetNode.right
        }
      }
    }
  }

  /**
    * 删除以node为根节点的树的最小节点，并返回该最小节点的值
    *
    * @param node
    * @return
    */
  def delMinNode(node: AVLNode): Int = {
    var targetNode = node
    //向左一直找，最左端的节点就是最小节点
    while (targetNode.left != null) {
      targetNode = targetNode.left
    }
    val minVal = targetNode.value
    //删除该最小节点
    delNode(minVal)
    minVal
  }

  /**
    * 查找指定节点
    *
    * @param value
    * @return
    */
  def searchNode(value: Int): AVLNode = {
    if (root != null) root.searchNode(value) else null
  }

  /**
    * 查找指定节点的父节点
    *
    * @param value
    * @return
    */
  def searchParnetNode(value: Int): AVLNode = {
    if (root != null) root.searchParentNode(value) else null
  }

  /**
    * 中序遍历
    */
  def middleOrder: Unit = {
    if (root != null) root.middleOrder else println("二叉排序树为空")
  }

  /**
    * 给排序二叉树添加节点
    *
    * @param node
    */
  def addNode(node: AVLNode): Unit = {
    if (root == null) root = node else root.addNode(node)
  }
}

class AVLNode(var value: Int) {
  var left: AVLNode = _
  var right: AVLNode = _

  /**
    * 左旋转
    */
  private def leftRotate: Unit = {
    //以当前根节点的值创建新的节点
    val newNode = new AVLNode(value)
    //把新节点的左子树设置成当前节点的左子树
    newNode.left = left
    //把新节点的右子树设置成当前节点的右子树的左子树
    newNode.right = right.left
    //把当前节点的值替换成右子节点的值
    value = right.value
    //把当前节点的右子树设置成当前节点右子树的右子树
    right = right.right
    //把当前节点的左子树设置成新的节点
    left = newNode
  }

  /**
    * 右旋转
    */
  private def rightRotate: Unit = {
    val newNode = new AVLNode(value)
    newNode.right = right
    newNode.left = left.right
    value = left.value
    left = left.left
    right = newNode
  }

  /**
    * 给当前节点添加子节点
    *
    * @param node 待添加节点
    */
  def addNode(node: AVLNode): Unit = {
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
    //添加完节点之后，如果（右子树的高度 - 左子树的高度) > 1, 左旋转
    if (rightHight - leftHight > 1) {
      //如果添加完之后，它的右子树的左子树的高的大于他的右子树的高度, 双旋转
      if (right != null && right.leftHight > right.rightHight) {
        //先对右子树进行右旋转
        right.rightRotate
        //在对整个树左旋转
        leftRotate
      } else {//左旋转
        leftRotate
      }
      return
    }

    //添加完节点之后，如果（左子树的高度 - 右子树的高度) > 1
    if (leftHight - rightHight > 1) {
      //如果添加完之后，它的左子树的右子树的高的大于他的左子树的高度, 双旋转
      if (left != null && left.rightHight > left.leftHight) {
        //先对左子树进行左旋转
        left.leftRotate
        //在对整个树右旋转
        rightRotate
      } else {//右旋转
        rightRotate
      }
    }
  }

  /**
    * 该节点右子树的高度
    *
    * @return
    */
  def rightHight: Int = {
    if (right != null) right.hight else 0
  }

  /**
    * 该节点左子树的高度
    *
    * @return
    */
  def leftHight: Int = {
    if (left != null) left.hight else 0
  }

  /**
    * 以该节点为根节点的子树的高度
    *
    * @return
    */
  def hight: Int = {
    Math.max(if (left == null) 0 else left.hight, if (right == null) 0 else right.hight) + 1
  }


  /**
    * 查找值为value的父节点
    *
    * @param value
    * @return
    */
  def searchParentNode(value: Int): AVLNode = {
    if ((left != null && left.value == value) || (right != null && right.value == value)) {
      return this
    }
    if (left != null && value < this.value) {
      return left.searchParentNode(value)
    }
    if (right != null && value >= this.value) {
      return right.searchParentNode(value)
    }
    null
  }

  /**
    * 查找值为value的节点
    *
    * @param value
    * @return
    */
  def searchNode(value: Int): AVLNode = {

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
    } else {
      //除此之外的情况说明没找到
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
}

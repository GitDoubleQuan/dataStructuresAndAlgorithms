package com.atguigu.search

import scala.collection.mutable.ArrayBuffer

object BinarySearch {

  def main(args: Array[String]): Unit = {
    val arr = Array[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 9)
    val finalVal = 9
    val indexs = binarySearch(arr, finalVal, 0, arr.length - 1)
    if (indexs != null) {
      println(s"index:${indexs.mkString(",")},val:${indexs.map(arr(_)).mkString(",")}")
    } else {
      println("找不到指定元素")
    }
  }

  /**
    *
    * @param arr      待查找序列
    * @param finalVal 要查找的元素
    * @param left     每轮要查找的序列范围 - 开始位置下标
    * @param right    每轮要查找的序列范围 - 结束位置下标
    * @return
    */
  def binarySearch(arr: Array[Int], finalVal: Int, left: Int, right: Int): ArrayBuffer[Int] = {
    //left > right说明已经超出了待查找序列的范围，可以跳出递归，序列中没有要查找的元素
    if (left > right) {
      return null
    }
    //找到有序序列的中间值
    val midIndex = (left + right) / 2
    val mid = arr(midIndex)
    //如果序列中间值等于要查找的元素，则返回该元素的下标
    if (mid == finalVal) {
      val indexs = ArrayBuffer[Int]()
      indexs += midIndex
      var rightIndex = midIndex + 1
      var leftIndex = midIndex - 1
      while (rightIndex <= right && arr(rightIndex) == finalVal) {
        indexs += rightIndex
        rightIndex += 1
      }
      while (leftIndex >= left && arr(leftIndex) == finalVal) {
        indexs += leftIndex
        leftIndex -= 1
      }
      return indexs
    } else {
      //如果要查找的元素大于中间值，则递归查找序列的右半部分
      if (finalVal > mid) {
        binarySearch(arr, finalVal, midIndex + 1, right)
      } else {
        //如果要查找的元素小于中间值，则递归查找序列的左半部分
        binarySearch(arr, finalVal, left, mid - 1)
      }
    }
  }
}

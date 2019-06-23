package com.atguigu.search

object InsertValSearch {

  def main(args: Array[String]): Unit = {
    val arr = (1 to 100).toArray
    val finalVal = 100
    val index = insertValSearch(arr,finalVal,0,arr.length - 1)
    println(s"index:$index")
  }

  def insertValSearch(arr: Array[Int], finalVal: Int, left: Int, right: Int): Int = {

    if (left > right || finalVal > arr(right) || finalVal < arr(left)) {
      return -1
    }

    //相对于二分查找只是改变了mid的取值公式
    val midIndex = left + (finalVal - arr(left)) / (arr(right) - arr(left)) * (right - left)
    val mid = arr(midIndex)

    if (mid == finalVal) {
      return midIndex
    } else {
      if (finalVal > mid) {
        insertValSearch(arr, finalVal, midIndex + 1, right)
      } else {
        insertValSearch(arr, finalVal, left, midIndex - 1)
      }
    }

  }
}

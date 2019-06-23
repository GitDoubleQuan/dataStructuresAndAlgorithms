package com.atguigu.sort


object InsertSort {
  def main(args: Array[String]): Unit = {
    val arr = Array[Int](5, 2, 3, 1, 4, 6)
    insertSort(arr)
    println(arr.mkString(" "))
  }

  def insertSort(arr:Array[Int]): Unit ={
    for (i <- 1 until arr.size) {
      val insertVal = arr(i)
      var insertIndex = i - 1
      while (insertIndex >= 0 && insertVal < arr(insertIndex)) {
        arr(insertIndex + 1) = arr(insertIndex)
        insertIndex = insertIndex - 1
      }
      arr(insertIndex + 1) = insertVal
    }
  }
}

package com.atguigu.sort

object MergetSort {
  def main(args: Array[String]): Unit = {
    val arr = Array[Int](7, 1, 4, 3, 9, 0, 2, 5)
    mergeSort(arr, 0, arr.size - 1)
    println(arr.mkString(" "))
  }

  /**
    * 递归拆分待排序序列，并在递归的过程中合并相邻的两个有序序列
    *
    * @param arr   待排序的数组
    * @param left  数组的起始下标
    * @param right 数组的结束下标
    */
  def mergeSort(arr: Array[Int], left: Int, right: Int): Unit = {

    //当分到子序列只有一个元素的时候，就可以停止拆分子序列了
    if (left < right) {
      val mid = (left + right) / 2
      //左递归：把拆分后的左边的子序列继续拆分
      mergeSort(arr, left, mid)
      //右递归：把拆分后的右边的子序列继续拆分
      mergeSort(arr, mid + 1, right)
      //在递归的过程中合并相邻的两个有序子序列
      merge(arr, left, mid, right)
    }
  }


  /**
    * 两个有序序列合并成一个有序序列
    *
    * @param arr   要排序的数组
    * @param left  左边有序序列的开始位置
    * @param mid   左边有序序列的结束位置
    * @param right 右边有序序列的结束位置
    */
  def merge(arr: Array[Int], left: Int, mid: Int, right: Int): Unit = {
    //定义临时数组用于存放取出的元素
    val temp = new Array[Int](right - left + 1)
    //临时数组的指针，代表元素放在临时数组中的下标位置
    var t = 0
    //左边有序序列的开始位置下标
    var i = left
    //右边有序序列开始位置下标
    var j = mid + 1
    //两个待合并的有序序列中有一个先被遍历完，就跳出循环
    while (i <= mid && j <= right) {
      //左边序列的i位置的元素大于右边序列j位置的元素，就把arr(i)放在临时数组的相应位置，并把指针i和t向后移动一位
      if (arr(i) <= arr(j)) {
        temp(t) = arr(i)
        i += 1
        t += 1
      } else {
        //右边序列的j位置的元素大于左边序列i位置的元素，就把arr(j)放在临时数组的相应位置，并把指针j和t向后移动一位
        temp(t) = arr(j)
        j += 1
        t += 1
      }
    }
    //此时左边序列和右边序列已经有一个已经遍历完，只要把左边序列或右边序列中剩下的元素放入临时数组中就行，
    //这样临时数组已经是合并了两个有序序列后的有序序列了
    while (i <= mid) {
      temp(t) = arr(i)
      i += 1
      t += 1
    }
    while (j <= right) {
      temp(t) = arr(j)
      j += 1
      j += 1
    }
    //把临时数组中的数据覆盖到待排序的原数组的相应位置
    var leftTemp = left
    for (i <- 0 until temp.size) {
      arr(leftTemp) = temp(i)
      leftTemp += 1
    }
  }
}

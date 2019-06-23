package com.atguigu.sort

import util.control.Breaks._

object QuickSort {
  def main(args: Array[String]): Unit = {
    val arr = Array[Int](1, 2, 3, 4, 0, 4, 5)
    quickSort(arr, 0, arr.size - 1)
    println(arr.mkString(" "))
  }

  /**
    *
    * @param arr   待排序数组
    * @param left  数组起始位置下标
    * @param right 数组介绍位置下标
    */
  def quickSort(arr: Array[Int], left: Int, right: Int): Unit = {
    var l = left
    var r = right

    //找一个基准
    val pivot = arr((l + r) / 2)

    var temp = 0
    //l和r相遇或者交叉时，说明一趟排序结束
    breakable {
      while (l < r) {
        //左边找到比基准大的数
        while (arr(l) < pivot) {
          l += 1
        }
        //右边找到比基准小的数
        while (arr(r) > pivot) {
          r -= 1
        }

        //如果l和r已经相遇或交叉，结束这趟排序
        if (l >= r) {
          break()
        }

        //交换左右两边的数
        temp = arr(l)
        arr(l) = arr(r)
        arr(r) = temp

        //为了防止这种情况：一个序列中存在重复元素，并且重复元素有可能在某趟排序中被选为基准，这样代码就会发生死循环
        //例如：1 2 3 4 0 4 5    基准为4
        //            l   r
        //交换之后，序列依然为
        //      1 2 3 4 0 4 5
        //            l   r
        if (arr(l) == pivot) {
          r -= 1
        }
        if (arr(r) == pivot) {
          l += 1
        }
      }
    }
    //如果l和r相遇，那相遇的那个数是基准，基准不用参与排序
    if (l == r) {
      l += 1
      r -= 1
    }

    //如果左边的序列元素个数大于一个，就继续递归排序左边的部分
    if (left < r) {
      quickSort(arr, left, r)
    }

    //如果右边的序列元素个数大于一个，继续递归排序右边的部分
    if (l < right) {
      quickSort(arr, l, right)
    }
  }
}

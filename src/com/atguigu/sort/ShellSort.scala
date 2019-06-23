package com.atguigu.sort

object ShellSort {

  def main(args: Array[String]): Unit = {
    val arr = Array[Int](8, 9, 1, 7, 2, 3, 5, 4, 6, 0)
    shellSort(arr)
  }

  def shellSort(arr: Array[Int]): Unit = {
    var temp = 0
    var count = 0
    //步长
    var gap = arr.length / 2
    //当分组只有一个的时候，步长为1，所以循环的结束条件是步长为小于等于0
    while (gap > 0) {
      for (i <- gap until arr.length) {
        for (j <- (i - gap) to(0, -gap)) {
          if (arr(j) > arr(j + gap)) {
            temp = arr(j)
            arr(j) = arr(j + gap)
            arr(j + gap) = temp
          }
        }
      }
      count += 1
      println(s"第${count}趟排序后：arr=${arr.mkString(" ")}")
      gap /= 2
    }
  }
}
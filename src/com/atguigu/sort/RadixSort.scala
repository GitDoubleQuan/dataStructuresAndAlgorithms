package com.atguigu.sort

object RadixSort {

  def main(args: Array[String]): Unit = {
    val arr = Array[Int](10, 4, 222, 45, 30, 1, 0)
    println(s"原始序列，arr=[${arr.mkString(" ")}]")
    radixSort(arr)
  }

  def radixSort(arr: Array[Int]): Unit = {
    //序列中最大的数
    var max = arr(0)
    arr.foreach(x => if (x > max) max = x)

    //最大的数有多少位，代表了要排序多少趟
    val maxDigitCount = max.toString.length

    //初始化桶，0 - 9共10个桶，每个桶的大小为序列的大小
    val buckets = new Array[Array[Int]](10).map(_ => new Array[Int](arr.length))

    //每趟排序的过程中，从桶中往外取数据时，每个桶中各存放的多少数据
    val bucketSize = new Array[Int](10)

    for (i <- 0 until maxDigitCount) {
      //把序列的元素依次取出放入桶里
      for (element <- arr) {
        //得到该元素相应数位的值
        val digit = element / Math.pow(10, i).toInt % 10
        //把该元素放在对应的桶里
        buckets(digit)(bucketSize(digit)) = element
        //添加了该元素的桶的size加1
        bucketSize(digit) = bucketSize(digit) + 1
      }
      //所有元素已经放入桶里，依次取出桶里的元素，覆盖原来的序列
      var index = 0
      for (k <- 0 until 10) {
        for (j <- 0 until bucketSize(k)) {
          arr(index) = buckets(k)(j)
          index += 1
        }
        //该桶中的元素取完，就把该桶清空，为下一趟排序准备
        bucketSize(k) = 0
      }
      println(s"第${i + 1}趟排序，arr=[${arr.mkString(" ")}]")
    }

  }

}

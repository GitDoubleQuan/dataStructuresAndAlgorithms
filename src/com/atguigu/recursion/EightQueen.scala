package com.atguigu.recursion

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object EightQueenTest {
  def main(args: Array[String]): Unit = {
    //    queen.diagonal.foreach { case (diagonalNo, elements) =>
    //      println(s"No:$diagonalNo  -> elements:${elements.mkString(";")}")
    //    }
    //    println(queen.layout.mkString(" "))
    //
    //    println(queen.canSet(0, 0))

    val queen = new EightQueen
    //    queen.simulation(0, 0)
    //    queen.print

    queen.simulation2(0)

    println(s"一共有${queen.method}种放置方法")

  }
}

class EightQueen {
  val chessboard = new Array[Int](8)

  var method = 0

  val layout = {
    new Array[Int](8).map(_ => -1)
  }

  val diagonal = {
    val diagonalMap = new mutable.HashMap[String, Array[Tuple2[Int, Int]]]
    for (j <- 0 until 15) {
      val prefix = "A"
      val diagonalNo = prefix + j
      val size = if (j <= 7) j + 1 else 15 - j
      val tuples = new Array[Tuple2[Int, Int]](size)
      diagonalMap.put(diagonalNo, tuples)
      var x = if (j <= 7) 0 else j - 7
      var y = j - x
      var index = 0
      tuples(index) = (x, y)
      while (index < size - 1) {
        x += 1
        y -= 1
        index += 1
        tuples(index) = (x, y)
      }
    }
    for (j <- 0 until 15) {
      val prefix = "B"
      val diagonalNo = prefix + j
      val size = if (j <= 7) j + 1 else 15 - j
      val tuples = new Array[Tuple2[Int, Int]](size)
      diagonalMap.put(diagonalNo, tuples)
      var x = if (j <= 7) 7 else 14 - j
      var y = if (j >= 7) 7 else j
      var index = 0
      tuples(index) = (x, y)
      while (index < size - 1) {
        x -= 1
        y -= 1
        index += 1
        tuples(index) = (x, y)
      }
    }
    diagonalMap
  }

  def canSet(i: Int, j: Int): Boolean = {
    if (i < 0 || i > 7 || j < 0 || j > 7) {
      return false
    }
    val exist = new ArrayBuffer[String]
    for (x <- 0 until 8) {
      val y = layout(x)
      if (y != -1) {
        exist += "X" + x
        exist += "Y" + y
        diagonal.filter { case (key, element) => element.contains((x, y)) }.map(_._1).foreach(exist += _)
      }
    }

    val currentDiagonals = diagonal.filter { case (key, element) => element.contains((i, j)) }.map(_._1)

    !exist.contains("X" + i) && !exist.contains("Y" + j) && currentDiagonals.filter(exist.contains(_)).size == 0
  }

  def canSet2(n: Int): Boolean = {

    for (x <- 0 until n) {
      if (layout(x) == layout(n) || Math.abs(n - x) == Math.abs(layout(n) - layout(x))) {
        return false
      }
    }
    true
  }

  //一种排放方法
  def simulation(i: Int, j: Int): Boolean = {
    if (layout(7) != -1) {
      return true
    } else {
      if (!canSet(i, j)) {
        //如果放子失败，在此行的向右的位置继续尝试
        if (j < 7) {
          simulation(i, j + 1)
        } else {
          //如果此行没有位置可以放子，则要调整上一行放子的位置
          val site = layout(i - 1)
          layout(i - 1) = -1
          simulation(i - 1, site + 1)
        }
      } else {
        layout(i) = j
        simulation(i + 1, 0)
        return true
      }
    }
  }

  //所有排放方法
  def simulation2(n: Int): Unit = {
    if (n == 8) {
      print
      method += 1
      return
    }
    for (i <- 0 until 8) {
      layout(n) = i
      if (canSet2(n)) {
        simulation2(n + 1)
      }
    }
  }

  def print: Unit = {
    println("======= " + layout.mkString(";") + " =======")
    val array = new Array[Array[Int]](8)
    for (i <- 0 until 8) {
      val ints = new Array[Int](8)
      ints(layout(i)) = 1
      array(i) = ints
    }
    for (i <- 7 to(0, -1)) {
      println(array(i).mkString("  "))
    }
  }


}



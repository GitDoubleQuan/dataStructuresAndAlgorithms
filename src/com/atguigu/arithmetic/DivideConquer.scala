package com.atguigu.arithmetic

object DivideConquer {

  def main(args: Array[String]): Unit = {
    hannuoTower(2, 'a', 'b', 'c')



  }

  /**
    * 模拟汉诺塔问题
    *
    * @param num 一共要移动几个盘
    * @param a   从哪个柱子移动
    * @param b   中间利用到哪个柱子
    * @param c   最终移动到哪个柱子
    */
  def hannuoTower(num: Int, a: Char, b: Char, c: Char): Unit = {
    //如果只有一个盘，只要把盘从a移动到c
    if (num == 1) {
      println(s"第$num 个盘：从$a -> $c")
    } else {
      //如果我们现有盘大于等于2,依然可以看成2个盘，1.最下面的一个盘 2.上面的所有盘
      //1.先把上面的所有盘从a移动到b,中间可能会用到c
      hannuoTower(num - 1, a, c, b)
      //2.把最下面的盘移动到c
      println(s"第$num 个盘：从$a -> $c")
      //3.把上面所有的盘移动到从b移动到c,中间可能用到a
      hannuoTower(num - 1, b, a, c)
    }
  }
}



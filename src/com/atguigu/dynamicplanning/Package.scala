package com.atguigu.dynamicplanning

/**
  * 背包问题
  */
object Package {

  def main(args: Array[String]): Unit = {
    //3种物品，重量
    val w = Array[Int](1, 4, 3)
    //3种物品，价值
    val v = Array[Int](1500, 3000, 2000)
    //背包容量
    val c = 4

    //假设背包容量是W，在前i件物品中挑选总重量不超过W，每种物品最多能放入一个，使得总价值最大；
    //这时最优值记作：m(i,W),其中 1<=i<=n,1<=W<=C
    //考虑第i个物品，无非两种情况，选或者不选
    //1.选的话，背包容量变小，改变问题P(i-1,W-w(i))
    //2.不选的话，背包容量不变，改变问题P(1-1,W)
    //所以，此时在放入第i个物品时的最优方案就是比较：
    //max(m(i-1,W),v(i) + m(i-1,W-w(i-1)))

    //把m函数用一个二维数组表示
    val mFunc = new Array[Array[Int]](w.length + 1).map(_ => new Array[Int](c + 1))
    //此二维数组记录放入物品的路径
    val path = new Array[Array[Int]](w.length + 1).map(_ => new Array[Int](c + 1))
    for (i <- 0 until mFunc.length) {
      mFunc(i)(0) = 0
    }
    for (j <- 0 until mFunc(0).length) {
      mFunc(0)(j) = 0
    }

    //第前几种物品
    for (i <- 1 to w.length) {
      //背包容量
      for (j <- 1 to c) {
        //如果第i种物品是能放入背包的
        if (w(i - 1) <= j) {
          //这种情况才代表放入了i物品,就在path中记录
          if(mFunc(i - 1)(j) < v(i - 1) + mFunc(i - 1)(j - w(i - 1))){
            mFunc(i)(j) = v(i - 1) + mFunc(i - 1)(j - w(i - 1))
            path(i)(j) = 1
          }else{
            mFunc(i)(j) = mFunc(i - 1)(j)
          }
        } else {
          //如果第i种物品是不能放入背包的
          mFunc(i)(j) = mFunc(i - 1)(j)
        }
      }
    }

    //m函数的结果打印一下
    for (i <- 0 until mFunc.length) {
      for (j <- 0 until mFunc(0).length) {
        print(mFunc(i)(j) + " ")
      }
      println()
    }

    //物品放入路径的结果打印一下
    for (i <- 0 until path.length) {
      for (j <- 0 until path(0).length) {
        print(path(i)(j) + " ")
      }
      println()
    }

    //背包中放入的是哪几种物品：
    //从背包容量最大，最后一个物品往前遍历
    var j = c //容量
    var i = w.length //物品
    while(j > 0){//容量
      while(i > 0 ){//物品
        if(path(i)(j) == 1){
          println(s"放入第$i 件物品")
          j -= w(i -1)
        }
        i -= 1
      }
    }
  }
}
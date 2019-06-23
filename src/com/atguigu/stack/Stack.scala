package com.atguigu.stack

import scala.collection.mutable
import util.control.Breaks._

object CalculatorTest {
  def main(args: Array[String]): Unit = {
    val calculator = new Calculator
    val result = calculator.calculate("11*11-10-20*1")
    println(result)
  }
}

class Calculator {
  val numStack: mutable.Stack[Int] = new mutable.Stack[Int]
  val operStack: mutable.Stack[String] = new mutable.Stack[String]
  var calculate: String = _

  def calculate(calculate: String): Int = {
    this.calculate = calculate
    var element = nextElement
    while (!"".equals(element)) {
      //如果当前元素是操作符
      if (elementIsOper(element)) {
        //如果当前操作符的优先级小于栈顶的操作符的优先级，从数栈中取出两个元素,运算，并把运算结果压入数栈
        if (operStack.isEmpty) {
          operStack.push(element)
        } else if (priority(element) <= priority(operStack.top.toString)) {
          val num1 = numStack.pop()
          val num2 = numStack.pop()
          val countRes = count(operStack.pop(), num1, num2)
          numStack.push(countRes)
          operStack.push(element)
        } else {
          //如果当前操作符的优先级大于栈顶的操作符的优先级，则操作符直接入栈
          operStack.push(element)
        }
        //如果当前元素是数值，这直接入栈
      } else {
        numStack.push(element.toInt)
      }
      element = nextElement
    }
    //把栈中剩余的元素取出计算，当数栈中只剩一个元素，符号栈中没有元素的时候，计算结束
    for (oper <- operStack) {
      val num1 = numStack.pop()
      val num2 = numStack.pop()
      val countRes = count(oper, num1, num2)
      numStack.push(countRes)
    }

    numStack.pop()

  }

  def nextElement: String = {
    if (calculate.isEmpty) {
      return ""
    }
    var nextElement = ""
    var index = 0
    if (isOper(calculate.charAt(0))) {
      nextElement = calculate.substring(0, 1)
      calculate = calculate.substring(1)
    } else {
      breakable {
        for (c <- calculate) {
          if (isOper(c)) {
            break()
          }
          nextElement += c
          index += 1
        }
      }
    }
    calculate = calculate.substring(index)
    nextElement
  }

  def isOper(c: Char) = {
    c == '+' || c == '-' || c == '*' || c == '/'
  }

  def isNoElement = {
    calculate.isEmpty
  }

  def elementIsOper(element: String) = {
    if (element.length == 1) {
      isOper(element.charAt(0))
    } else {
      false
    }
  }

  //优先级越高数值越大
  def priority(oper: String) = {
    oper match {
      case "*" => 1
      case "/" => 1
      case "+" => 0
      case "-" => 0
      case _ => -1
    }
  }

  //计算
  def count(oper: String, num1: Int, num2: Int): Int = {
    oper match {
      case "+" => num1 + num2
      case "-" => num2 - num1
      case "*" => num1 * num2
      case "/" => num2 / num1
    }
  }


}

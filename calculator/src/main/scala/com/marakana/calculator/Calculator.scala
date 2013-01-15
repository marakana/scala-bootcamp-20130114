package com.marakana.calculator

import collection.mutable

object Calculator {

  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      println("usage: Calculator <expression>")
    else {
      val expression = args(0)
      val tokens = expression.split(" ")
      val stack = new mutable.Stack[Int]()
      for (token <- tokens) token match {
        case "+" => {
          val rhs = stack.pop()
          val lhs = stack.pop()
          stack.push(lhs + rhs)
        }
        case "-" => {
          val rhs = stack.pop()
          val lhs = stack.pop()
          stack.push(lhs - rhs)
        }
        case "*" => {
          val rhs = stack.pop()
          val lhs = stack.pop()
          stack.push(lhs * rhs)
        }
        case "/" => {
          val rhs = stack.pop()
          val lhs = stack.pop()
          stack.push(lhs / rhs)
        }
        case _   => stack.push(token.toInt)
      }
      println(stack.pop())
    }
  }
}

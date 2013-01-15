package com.marakana.calculator

import collection.mutable

object Calculator {

  def handleOperator(token: String, stack: mutable.Stack[Int]): Boolean = token match {
    case "+" => {
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs + rhs)
      true
    }
    case "-" => {
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs - rhs)
      true
    }
    case "*" => {
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs * rhs)
      true
    }
    case "/" => {
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs / rhs)
      true
    }
    case _ => false
  }

  def handleNumber(token: String, stack: mutable.Stack[Int]): Boolean = try {
    stack.push(token.toInt)
    true
  } catch {
    case _: NumberFormatException => false
  }

  def calculate(expression: String): Int = {
    val tokens = expression.split(" ")
    val stack = new mutable.Stack[Int]()
    for (token <- tokens) {
      if (!handleNumber(token, stack) && !handleOperator(token, stack)) {
        throw new IllegalArgumentException("invalid expression")
      }
    }
    stack.pop()
  }

  def main(args: Array[String]): Unit = args match {
    case Array(expression) => println(calculate(expression))
    case _ => println("usage: Calculator <expression>")
  }
}

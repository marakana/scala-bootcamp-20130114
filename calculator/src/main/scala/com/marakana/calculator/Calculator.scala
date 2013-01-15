package com.marakana.calculator

import collection.mutable

object Calculator {

  object Operator {
    private val operators: Map[String, (Int, Int) => Int] = Map(
      "+" -> { _ + _ },
      "-" -> { _ - _ },
      "*" -> { _ * _ },
      "/" -> { _ / _ })
    def unapply(token: String): Option[(Int, Int) => Int] = operators.get(token)
  }

  def handleOperator(token: String, stack: mutable.Stack[Int]): Boolean =
    token match {
      case Operator(op) =>
        val rhs = stack.pop()
        val lhs = stack.pop()
        stack.push(op(lhs, rhs))
        true
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

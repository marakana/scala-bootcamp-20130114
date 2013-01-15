package com.marakana.calculator

import collection.mutable

object Calculator {

  trait Operator {
    def operate(lhs: Int, rhs: Int): Int
  }
  class Add extends Operator {
    override def operate(lhs: Int, rhs: Int): Int = lhs + rhs
  }
  class Subtract extends Operator {
    override def operate(lhs: Int, rhs: Int): Int = lhs - rhs
  }
  class Multiply extends Operator {
    override def operate(lhs: Int, rhs: Int): Int = lhs * rhs
  }
  class Divide extends Operator {
    override def operate(lhs: Int, rhs: Int): Int = lhs / rhs
  }

  val operators: Map[String, Operator] = Map(
    "+" -> new Add,
    "-" -> new Subtract,
    "*" -> new Multiply,
    "/" -> new Divide)

  def handleOperator(token: String, stack: mutable.Stack[Int]): Boolean =
    operators.get(token) match {
      case Some(op) =>
        val rhs = stack.pop()
        val lhs = stack.pop()
        stack.push(op.operate(lhs, rhs))
        true
      case None => false
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

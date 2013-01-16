package com.marakana.calculator

import util.control.Exception.catching

object Calculator {

  object Operator {
    private val operators: Map[String, (Int, Int) => Int] = Map(
      "+" -> { _ + _ },
      "-" -> { _ - _ },
      "*" -> { _ * _ },
      "/" -> { _ / _ })
    def unapply(token: String): Option[(Int, Int) => Int] = operators.get(token)
  }

  object Number {
    def unapply(token: String): Option[Int] =
      catching(classOf[NumberFormatException]) opt token.toInt
  }

  def calculate(expression: String): Int = {
    val tokens = expression split " "
    val stack = tokens.foldLeft(List.empty[Int]) {
      (stack: List[Int], token: String) => token match {
        case Number(num) => num :: stack
        case Operator(op) => stack match {
          case rhs :: lhs :: stack => op(lhs, rhs) :: stack
          case _ => throw new IllegalArgumentException("wrong number of operands")
        }
        case _ => throw new IllegalArgumentException("invalid token")
      }
    }
    stack.head
  }

  def calculateOption(expression: String): Option[Int] = {
    val tokens = expression split " "
    val stack = tokens.foldLeft(Option(List.empty[Int])) {
      (stack: Option[List[Int]], token: String) => token match {
        case Number(num) => stack map { num :: _ }
        case Operator(op) => stack flatMap {
          case rhs :: lhs :: stack => Some(op(lhs, rhs) :: stack)
          case _ => None
        }
        case _ => None
      }
    }
    stack map { _.head }
  }

  def main(args: Array[String]): Unit = args match {
    case Array(expression) => println(calculateOption(expression))
    case _ => println("usage: Calculator <expression>")
  }
}

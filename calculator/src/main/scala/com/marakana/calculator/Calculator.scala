package com.marakana.calculator

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
    def unapply(token: String): Option[Int] = try {
      Some(token.toInt)
    } catch {
      case _: NumberFormatException => None
    }
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

  def main(args: Array[String]): Unit = args match {
    case Array(expression) => println(calculate(expression))
    case _ => println("usage: Calculator <expression>")
  }
}

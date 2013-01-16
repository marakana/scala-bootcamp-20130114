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

  def calculate(tokens: List[String], stack: List[Int]): Int = tokens match {
    case Number(num) :: tokens => calculate(tokens, num :: stack)
    case Operator(op) :: tokens => stack match {
      case rhs :: lhs :: stack => calculate(tokens, op(lhs, rhs) :: stack)
      case _ => throw new IllegalArgumentException("wrong number of operands")
    }
    case Nil => stack match {
      case head :: Nil => head
      case _ => throw new IllegalArgumentException("wrong number of operands")
    }
    case _ => throw new IllegalArgumentException("invalid token")
  }

  def calculate(expression: String): Int =
    calculate(expression.split(" ").toList, Nil)

  def main(args: Array[String]): Unit = args match {
    case Array(expression) => println(calculate(expression))
    case _ => println("usage: Calculator <expression>")
  }
}

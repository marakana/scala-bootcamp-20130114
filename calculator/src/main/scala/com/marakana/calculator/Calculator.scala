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

  def applyToken(stack: List[Int], token: String): Option[List[Int]] = token match {
    case Number(num) => Some(num :: stack)
    case Operator(op) => stack match {
      case rhs :: lhs :: stack => Some(op(lhs, rhs) :: stack)
      case _ => None
    }
    case _ => None
  }

  def calculateOption(expression: String): Option[Int] = {
    import scalaz.std.option._
    import scalaz.std.iterable._
    import scalaz.syntax.foldable._

    val tokens: Iterable[String] = expression split " "
    val stack = tokens.foldLeftM(List.empty[Int])(applyToken)
    stack map { _.head }
  }

  def main(args: Array[String]): Unit = args match {
    case Array(expression) => println(calculateOption(expression))
    case _ => println("usage: Calculator <expression>")
  }
}

package com.marakana.calculator

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class CalculatorSpec extends Specification with ScalaCheck {

  sealed trait Operator {
    def token: String
    def apply(lhs: Int, rhs: Int): Int
  }
  object Operator {
    case object Add extends Operator {
      val token: String = "+"
      def apply(lhs: Int, rhs: Int): Int = lhs + rhs
    }
    case object Subtract extends Operator {
      val token: String = "-"
      def apply(lhs: Int, rhs: Int): Int = lhs - rhs
    }
    case object Multiply extends Operator {
      val token: String = "*"
      def apply(lhs: Int, rhs: Int): Int = lhs * rhs
    }
    case object Divide extends Operator {
      val token: String = "/"
      def apply(lhs: Int, rhs: Int): Int = lhs / rhs
    }

    implicit val arbitraryOperator: Arbitrary[Operator] = Arbitrary(Gen.oneOf(Add, Subtract, Multiply, Divide))
  }

  "Calculator" should {
    import Calculator.calculate

    "operate on any two numbers correctly" in check { (lhs: Int, rhs: Int, op: Operator) =>
      (rhs != 0 || op != Operator.Divide) ==> {
        calculate(s"${lhs} ${rhs} ${op.token}") must beSome(op(lhs, rhs))
      }
    }

    "fail on missing operands" in {
      calculate("+") must beNone
    }

    "fail on garbage" in {
      calculate("lol") must beNone
    }
  }
}

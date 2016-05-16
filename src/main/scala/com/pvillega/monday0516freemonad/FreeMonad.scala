package com.pvillega.monday0516freemonad

import cats.{Id, Monoid, ~>}
import cats.free.Free
import cats.free.Free._
import cats.std.int._
import cats.std.string._


sealed trait Expression[A]

object Expression {

  case class Val[A](a: A) extends Expression[A]

  case class Plus[A: Monoid](exp1: Expression[A], exp2: Expression[A]) extends Expression[A] {
    val M = implicitly[Monoid[A]]
  }

  implicit def toVal[A](a: A) = Val(a)

  type ExpressionF[A] = Free[Expression, A]

  def value[A: Monoid](a: A): ExpressionF[A] =
    liftF[Expression, A](Val(a))

  def plus[A: Monoid](exp1: Expression[A], exp2: Expression[A]): ExpressionF[A] =
    liftF[Expression, A](Plus(exp1, exp2))

  val program: ExpressionF[Int] = for {
    a <- value(1)
    b <- value(2)
    c <- plus(a, b)
  } yield c

  val program2: ExpressionF[String] = for {
    a <- value("1")
    b <- value("2")
    c <- plus(a, b)
  } yield c

  def program3[T: Monoid](a0: T, b0: T) = for {
    a <- value(a0)
    b <- value(b0)
    c <- plus(a, b)
  } yield c

  def evaluate[B](program: ExpressionF[B]) = {

    def interpreter = new (Expression ~> Id) {

      def apply[A](fa: Expression[A]): Id[A] = {
        fa match {
          case Val(a) => a
          case p @ Plus(exp1, exp2) =>
            p.M.combine(apply(exp1), apply(exp2))
        }
      }
    }

    program.foldMap(interpreter)
  }

}

object Main3 extends App {

  import Expression._

  println(evaluate(program))
  println(evaluate(program2))
  println(evaluate(program3(1,2)))

}

package semigroupal

import cats.Semigroupal
import cats.syntax.parallel._


object parallel extends App {
  type ErrorOr[A] = Either[Vector[String], A]
  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))

  println(Semigroupal[ErrorOr].product(error1, error2))
  println((error1, error2).parTupled)

  println((List(1, 2), List(3, 4)).parTupled)
}

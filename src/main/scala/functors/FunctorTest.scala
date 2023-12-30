package functors

import cats.Functor
import cats.instances.function._
import cats.syntax.functor._


final case class Box[A](value: A)

object Box {
  // Implicit Functor instance for Box
  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    def map[A, B](initBox: Box[A])(f: A => B): Box[B] = Box(f(initBox.value))
  }
}

object FunctorTest extends App{
  val func1 = (a: Int) => a +1
  val func2 = (a: Int) => s"${a}!"
  val func3 = func1.map(func2)
  println(func3(1))

  val box = Box[Int](123)
  println(box.map(value => value + 1))
}

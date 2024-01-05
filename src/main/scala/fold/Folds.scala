package fold

import monoids.Monoid

object Folds extends App{

  println(List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a))
  println(List(1, 2, 3).foldRight(List.empty[Int])((a, i) => a :: i))

  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) :: accum
    }

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) ::: accum
    }

  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) { (item, accum) =>
      if (func(item)) item :: accum else accum
    }

  def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)

  println(map(List(1, 2, 3))(_ * 2))
  println(flatMap(List(1, 2, 3))(a => List(a, a * 10)))
  println(filter(List(1, 2, 3))(_ % 2 == 1))

  implicit val intSumMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }

  println(sumWithMonoid(List(1, 2, 3)))
}

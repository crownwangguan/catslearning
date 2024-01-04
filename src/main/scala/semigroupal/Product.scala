package semigroupal

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

object Product extends App {
  def product[F[_]: Monad, A, B](x: F[A], y: F[B]): F[(A, B)] =
    x.flatMap(a => y.map(b => (a, b)))

  println(product(List(1, 2), List(3, 4)))
}

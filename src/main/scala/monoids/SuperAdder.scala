package monoids

object SuperAdder {
  def add(items: List[Int]): Int =
    items.foldLeft(0)(_ + _) // can be rewrite to items.sum

  import cats.Monoid
  import cats.instances.int._
  import cats.syntax.semigroup._
  def monoidAdd(items: List[Int]): Int =
    items.foldLeft(Monoid[Int].empty)(_ |+| _)

  def monoidOptionAdd[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)

  implicit val orderMonoid: Monoid[Order] =
    new Monoid[Order] {
      override def empty: Order = Order(0, 0)
      override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }
}

case class Order(totalCost: Double, quantity: Double)

object Test extends App{
  import SuperAdder.monoidOptionAdd
  println(monoidOptionAdd(List(1, 2, 3)))

  import cats.instances.option._
  println(monoidOptionAdd(List(Option(1), Option(2), Option(3))))

  import SuperAdder.orderMonoid
  println(monoidOptionAdd(List(Order(1, 2), Order(3, 4), Order(5, 6))))
}
package functors

import cats.Functor
import cats.implicits.toFunctorOps

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
  }
}

object TreeTest extends App {
  import Tree._
  
  val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
  val resultTree: Tree[Int] = tree.map(_ * 2)
  println(resultTree)
}

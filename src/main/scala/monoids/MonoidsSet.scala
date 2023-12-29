package monoids

object MonoidsSet {
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
    }

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
    }

  implicit def symDiffMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
    }
}

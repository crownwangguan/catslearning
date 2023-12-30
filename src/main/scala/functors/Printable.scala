package functors


trait Printable[A] { self =>
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    override def format(value: B): String = self.format(func(value))
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        s"'${value}'"
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if(value) "yes" else "no"
    }

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)
}

object PrintableTest extends App {
  import Printable._

  println(format("Hello"))
  println(format(true))
  println(format(Box(true)))
  println(format(Box("Hello")))
}

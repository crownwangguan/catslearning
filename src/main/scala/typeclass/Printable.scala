package typeclass

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    (value: String) => value
  implicit val intPrintable: Printable[Int] = {
    (value: Int) => value.toString
  }
  implicit val catPrintable: Printable[Cat] = {
    (value: Cat) => s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String =
    printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit =
    println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String =
      printable.format(value)

    def print(implicit printable: Printable[A]): Unit =
      println(format(printable))
  }
}


final case class Cat(name: String, age: Int, color: String)

object PrintableTest extends App {
  import PrintableInstances._
  import PrintableSyntax._


  import cats._
  import cats.implicits._

  implicit val catShow: Show[Cat] =
    Show.show(value => s"${value.name} is a ${value.age} year-old ${value.color} cat.")

  val cat = Cat("mike", 12, "black")
  println(cat.show)

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name &&
        cat1.age === cat2.age &&
        cat1.color === cat2.color
    }

  val cat2 = Cat("mii", 12, "black")
  println(cat === cat2)
}

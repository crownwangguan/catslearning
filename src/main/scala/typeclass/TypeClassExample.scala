package typeclass

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNull() extends Json

final case class Person(name: String, email: String)

trait JsonWriter[A] {
  def write(value: A): Json
}

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] =
    (value: Person) => JsObject(
      Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      )
    )

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      override def write(value: Option[A]): Json =
        value match {
          case Some(aValue) => writer.write(aValue)
          case None => JsNull()
        }
    }
}

// Interface Object
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

// Interface Syntax
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

object Main extends App {
  import JsonWriterInstances._

  println(Json.toJson(Person("Guan", "guan@example.com"))) // Interface Object

  import JsonSyntax._

  println(Person("Guan", "guan@example.com").toJson) // Interface Syntax

  println(Option("A string").toJson)
}

package models

object ADTs {

  //Custom data type to showcase product (tuples too would work just fine)
  case class Employee(name: String, number: Int, manager: Boolean)
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  case class Laptop(name: String, number: Int, isWindows: Boolean)
  case class Attire(name: String, isFormal: Boolean)

  //showcase coproduct (Either[L, R]) too would work just fine
  sealed trait Shape
  case class Rectangle(width: Double, height: Double) extends Shape
  case class Circle(radius: Double) extends Shape

  //recursive type (requires lazy evaluation to avoid compiler's assumption of implicit divergence)
  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  case class Vec(x: Int, y: Int)
  case class Rect(origin: Vec, size: Vec)

  case class Wrapper(value: Int)

  // for tagged types and phantom types
  sealed trait JsonValue
  case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
  case class JsonArray(items: List[JsonValue]) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonString(value: String) extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

}

package object MigrationADTs{
  case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)
  case class IceCreamV2a(name: String, numCherries: Int)                                  // Remove field
  case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)                 // Reorder field
  case class IceCreamV2c(name: String, numCherries: Int, inCone: Boolean, numWaffles: Int)  // Insert field
}

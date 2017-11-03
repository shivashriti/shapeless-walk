package models

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, CNil, Coproduct, Inr, Inl, :+:}

// for tagged types and phantom types
sealed trait JsonValue
case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
case class JsonArray(items: List[JsonValue]) extends JsonValue
case class JsonNumber(value: Double) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case object JsonNull extends JsonValue

trait JsonEncoder[A]{
  def encode(value: A): JsonValue
}

trait JsonObjectEncoder[A] extends JsonEncoder[A]{
  def encode(value: A): JsonObject
}

object JsonEncoder{
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] = new JsonEncoder[A] {
    def encode(value: A): JsonValue = func(value)
  }

  implicit val stringEncoder: JsonEncoder[String] = createEncoder(str => JsonString(str))
  implicit val doubleEncoder: JsonEncoder[Double] =createEncoder(d => JsonNumber(d))
  implicit val booleanEncoder: JsonEncoder[Boolean] = createEncoder(b => JsonBoolean(b))
  implicit val intEncoder: JsonEncoder[Int] = createEncoder(i => JsonNumber(i))

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(list => JsonArray(list.map(value => enc.encode(value))))

  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(value => value.map(enc.encode).getOrElse(JsonNull))

  def createObjectEncoder[A](func: A => JsonObject): JsonObjectEncoder[A] = new JsonObjectEncoder[A] {
    def encode(value: A): JsonObject = func(value)
  }

  implicit val hnilObjectEncoder: JsonObjectEncoder[HNil] = createObjectEncoder(hnil => JsonObject(Nil))

  implicit def hlistObjectEncoder[H, T <: HList, K <: Symbol](
    implicit
    witness: Witness.Aux[K],        // to get field-name (value associated with K in FieldType[K, L])
    hEncoder: Lazy[JsonEncoder[H]], // Lazy defers implicit evaluation at runtime, guarding from compiler's assumption of implicit divergence
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    createObjectEncoder { hl =>
      val head = hEncoder.value.encode(hl.head)
      val tail = tEncoder.encode(hl.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] = createObjectEncoder(cnil => throw new Exception("Inconceivable"))

  implicit def coproductObjectEncoder[K <: Symbol, L, R <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    lEncoder: Lazy[JsonEncoder[L]],
    rEncoder: JsonObjectEncoder[R]
  ): JsonObjectEncoder[FieldType[K, L] :+: R] = {
    val typeName = witness.value.name
    createObjectEncoder{
      case Inl(l) => JsonObject(List((typeName, lEncoder.value.encode(l))))
      case Inr(r) => rEncoder.encode(r)
    }
  }

  implicit def genericObjectEncoder[A, H](
    implicit
    gen: LabelledGeneric.Aux[A, H],   // LabelledGeneric uses Records (HList of FieldType) to tag items of product
    enc: Lazy[JsonObjectEncoder[H]]   // defer implicit evaluation by Lazy for HList's head (or Coproduct's left) and Generic's Repr
  ): JsonEncoder[A] =
    createObjectEncoder(value => enc.value.encode(gen.to(value)))


  /**
    * History:
    * type FieldType[K, V] = V with KeyTag[K, V]
    * ->> tags the type of a field with it's literal, KeyTag["IdentifierName", IdentifierType] : Phantom type i.e. No runtime semantics
    * 'field' encodes supplied value with singleton type of it's key
    * witness provides value for a singleton type
    * Use combination of Witness and FieldType to produce all the Ryan Gosling
    */
}

object JsonValue{

  def makeString(jValue: JsonValue): String = jValue match {
    case JsonArray(items) => "[" + items.map(makeString(_)).mkString(",") + "]"
    case JsonObject(fields) => "{" + fields.map{case (name, value) => ("\"" + name + "\"" + " : " + makeString(value))}.mkString(",") + "}"
    case JsonNumber(value) => value.toString
    case JsonBoolean(value) => value.toString
    case JsonString(value) => "\"" + value + "\""   //possibly buggy
    case JsonNull => "null"
  }
}

object Json extends App{
  import models.ADTs._

  implicit class JsonString[A](a: A){
    def getJsonString(implicit jsonEncoder: JsonEncoder[A]): String = JsonValue.makeString(jsonEncoder.encode(a))
  }

  val ic: IceCream = IceCream("sai", 90, false)
  val shape: Shape = Circle(1.0)
  val shapes: List[Shape] = List(Rectangle(1, 2), Circle(3), Rectangle(4, 5), Circle(6))
  val optShapes: List[Option[Shape]] = List(Some(Rectangle(1, 2)), None, Some(Rectangle(4, 5)), Some(Circle(6)))

  println(JsonEncoder[IceCream].encode(ic))
  println(JsonEncoder[Shape].encode(shape))

  println(ic.getJsonString)
  println(shapes.getJsonString)
  println(optShapes.getJsonString)
}
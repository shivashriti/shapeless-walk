package models

import models.ADTs._

trait JsonEncoder[A]{
  def encode(value: A): JsonValue
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
}

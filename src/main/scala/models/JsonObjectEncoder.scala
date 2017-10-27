package models

import models.ADTs._
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, CNil, Coproduct, Inr, Inl, :+:}

trait JsonObjectEncoder[A] extends JsonEncoder[A]{
  def encode(value: A): JsonObject
}

object JsonObjectEncoder{
  def createObjectEncoder[A](func: A => JsonObject): JsonObjectEncoder[A] = new JsonObjectEncoder[A] {
    def encode(value: A): JsonObject = func(value)
  }

  implicit val hnilObjectEncoder: JsonObjectEncoder[HNil] = createObjectEncoder(hnil => JsonObject(Nil))

  implicit def hlistObjectEncoder[H, T <: HList, K <: Symbol](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
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
      case Inl(l) => JsonObject(List(typeName -> lEncoder.value.encode(l)))
      case Inr(r) => rEncoder.encode(r)
    }
  }

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
    gen: LabelledGeneric.Aux[A, H],
    enc: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder(value => enc.value.encode(gen.to(value)))
}
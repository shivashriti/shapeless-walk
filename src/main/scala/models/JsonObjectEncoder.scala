package models

import models.ADTs._
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, CNil, Coproduct, Inr, Inl, :+:}

trait JsonObjectEncoder[A] extends JsonEncoder[A]{
  def encode(value: A): JsonObject
}

object JsonObjectEncoder{
  def apply[A](implicit enc: JsonObjectEncoder[A]): JsonObjectEncoder[A] = enc

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

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
    gen: LabelledGeneric.Aux[A, H],   // LabelledGeneric uses Records (HList of FieldType) to tag items of product
    enc: Lazy[JsonObjectEncoder[H]]   // defer implicit evaluation by Lazy for HList's head and Generic's Repr
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
package models

import models.SeparateEncoders._
import models.ADTs._
import shapeless.{HList, ::, HNil}
import shapeless.{CNil, :+:, Inl, Inr, Coproduct}
import shapeless.{Generic, Lazy}

/**
  * Created by Shiva on 11/10/2017.
  */
object CsvEncoder {
  //summoner method (preferred over implicitly)
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

  //instance method
  def createEncoder[A](func: A => List[String]): CsvEncoder[A] = new CsvEncoder[A]{
    override def encode(value: A): List[String] = func(value)
  }

  //instance constructors for basic types
  implicit val intEncoder: CsvEncoder[Int] = createEncoder(i => List(i.toString))
  implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(b => List(if(b) "yes" else "No"))
  implicit val stringEncoder: CsvEncoder[String] = createEncoder(s => List(s))
  implicit val doubleEncoder: CsvEncoder[Double] = createEncoder((d => List(d.toString)))

  //instance constructors for HList and HNil
  implicit val HNilEncoder: CsvEncoder[HNil] = createEncoder(hNil => Nil)

  implicit def HListEncoder[H, T <: HList](implicit hEncoder: Lazy[CsvEncoder[H]], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] =
    createEncoder{ case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)}

  implicit val laptopEncoder: CsvEncoder[Laptop] = {
    val gen = Generic[Laptop]
    val enc = CsvEncoder[gen.Repr]
    createEncoder(laptop => enc.encode(gen.to(laptop)))
  }

  //instance constructors for Coproduct
  implicit val CNilEncoder: CsvEncoder[CNil] =
    createEncoder(cnil => throw new Exception("Inconceivable")) //won't ever reach here

  implicit def CoproductEncoder[L, R <: Coproduct](implicit lEncoder: Lazy[CsvEncoder[L]], rEncoder: CsvEncoder[R]): CsvEncoder[L :+: R] =
    createEncoder{
      case Inl(l) => lEncoder.value.encode(l)
      case Inr(r) => rEncoder.encode(r)
    }

  //generic instance for all ADTs
  implicit def genericEncoder[A, R](implicit gen: Generic.Aux[A, R], enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] =
    createEncoder(a => enc.value.encode(gen.to(a)))

  /**
    * Summary:
    * 1. define type class
    * 2. define primitive instances
    * 3. define instances for HList and Coproduct as required
    * 4. define instance for generic
    * 5. go home.
    */
}

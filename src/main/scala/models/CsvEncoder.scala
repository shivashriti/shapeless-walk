package models

import models.ADTs._
import shapeless.{HList, ::, HNil}
import shapeless.{CNil, :+:, Inl, Inr, Coproduct}
import shapeless.{Generic, Lazy}

/**
  * Created by Shiva on 11/10/2017.
  */

//turn a value of type A into a row of CSV file
trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  //summoner method (preferred over implicitly)
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  /*
  // manual instance for custom data type.. OMG! never
  implicit val employeeEncoder: CsvEncoder[Employee] = new CsvEncoder[Employee] {
    override def encode(value: Employee): List[String] = List(
      value.name, value.number.toString, if (value.manager) "yes" else "no"
    )
  }

  // tuple encoder.. not needed anymore
    implicit def pairEncoder[A, B](implicit aEncoder: CsvEncoder[A], bEncoder: CsvEncoder[B]): CsvEncoder[(A, B)] =
    new CsvEncoder[(A, B)] {
      def encode(value: (A, B)): List[String] = aEncoder.encode(value._1) ++ bEncoder.encode(value._2)
    }
  */

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

  /*
  // single instance with generic Repr.. No Thanks
  implicit val laptopEncoder: CsvEncoder[Laptop] = {
    val gen = Generic[Laptop]
    val enc = CsvEncoder[gen.Repr]
    createEncoder(laptop => enc.encode(gen.to(laptop)))
  }
  */

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

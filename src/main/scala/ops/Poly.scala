package ops

import shapeless.{Poly1, Poly2, HNil, ::}

import scala.math.Numeric

/**
  * polymorphic functions.. meh!
  */
object total extends Poly1{
  implicit def base[A](implicit num: Numeric[A]): Case.Aux[A, Double] = at(num.toDouble)
  implicit def option[A](implicit num: Numeric[A]): Case.Aux[Option[A], Double] = at(o => o.map(num.toDouble).getOrElse(0.0))
  implicit def list[A](implicit num: Numeric[A]): Case.Aux[List[A], Double] = at(l => num.toDouble(l.sum))
}

/**
  * using functional operations implemented as ops
  */
// map
object sizeOf extends Poly1{
  implicit val intCase: Case.Aux[Int, Int] = at(identity)
  implicit val stringCase: Case.Aux[String, Int] = at(_.length)
  implicit val booleanCase: Case.Aux[Boolean, Int] = at(if(_) 1 else 0)
}

// flatMap
object valueAndSizeOf extends Poly1{
  implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] = at(i => i :: i :: HNil)
  implicit val stringCase: Case.Aux[String, String :: Int :: HNil] = at(s => s :: s.length :: HNil)
  implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int ::HNil] = at(b => b :: (if(b) 1 else 0) :: HNil)
}

// foldLeft and foldRight
object sum extends Poly2{
  implicit val intIntCase: Case.Aux[Int, Int, Int] = at((a,b) => a + b)
  implicit val intStringCase: Case.Aux[Int, String, Int] = at((a,b) => a + b.length)
  implicit val intBooleanCase: Case.Aux[Int, Boolean, Int] = at((a,b) => a + (if(b) 1 else 0))
}

object PolyDemo extends App{
  println(total(77))
  println(total(Option(10.0)))
  println(total(List(1, 2, 3L)))

  println((1 :: true :: "hey" :: HNil).map(sizeOf))
  println((1 :: true :: "hey" :: HNil).flatMap(valueAndSizeOf))
  println((1 :: true :: "hey" :: HNil).foldLeft(0)(sum))
}


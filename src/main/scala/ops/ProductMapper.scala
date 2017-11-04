package ops

import shapeless._
import shapeless.ops.hlist

/**
  * defining type classes using Poly
  * using Mapper and FlatMapper
  */
trait ProductMapper[A, B, P]{
  def apply(a: A): B
}

object ProductMapper{

  implicit def genericProductMapper[A, B, P <: Poly, ARepr <: HList, BRepr <: HList](
    implicit
     aGen: Generic.Aux[A, ARepr],
     bGen: Generic.Aux[B, BRepr],
     mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
     ): ProductMapper[A, B, P] =
    new ProductMapper[A, B, P] {
      def apply(a: A): B = bGen.from(mapper.apply(aGen.to(a)))
    }

  /**
    * defining mapTo directly without builder doesn't work DUH!
    * mapTo provides B type and further calls Builder.apply to specify Poly
    */
  implicit class ProductMapperOps[A](a: A){
    class Builder[B]{
      def apply[P <: Poly](poly: P)(implicit mapper: ProductMapper[A, B, P]): B = mapper.apply(a)
    }
    def mapTo[B]: Builder[B] = new Builder[B]
  }

  object conversions extends Poly1{
    implicit val intCase: Case.Aux[Int, Boolean] = at(_ > 0)
    implicit val booleanCase: Case.Aux[Boolean, Int] = at(if(_) 1 else 0)
    implicit val stringCase: Case.Aux[String, String] = at(identity)
  }
}

object ProductMapperDemo extends App{
  import ProductMapper._

  case class IceCream1(name: String, numCherries: Int, inCone: Boolean)
  case class IceCream2(name: String,  inCone: Boolean, numCherries: Int)

  println(IceCream1("sundae", 5, false).mapTo[IceCream2](conversions))
}
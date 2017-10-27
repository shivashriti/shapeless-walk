package models

import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.{IsHCons, Last}

/**
  * dependently typed function
  * return the second element in an HList
  */

trait Second[L <: HList] {
  type Out
  def apply(value: L): Out
}

object Second{
  type Aux[L <: HList, O] = Second[L]
  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst

  /**
    * type parameters are for input, type members are for output
    * Returning type Aux[L, inst.Out] preserves the out type (O shouldn't be erased)
    * this is where it differs from "implicitly"
    */

  //instance constructor for HList (should have at least 2 elements)
  implicit def hListSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
    new Second[A :: B :: Rest]{
      type Out = B
      def apply(value: A :: B :: Rest): B = value.tail.head
    }

  /**
    * Chaining dependent functions
    * Using generic to get Repr for case class and use Last to get type of it's last element
    * This method should not be in this companion object, but who cares
    */

  def lastField[A, Repr <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  /**
    * shapeless' generalized type constraints
    * summon a Generic for a case class with exactly one field
    */

  def getWrappedValue[A, Head, Repr <: HList, Tail <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],  //find a generic with suitable Repr of A
    isHCons: IsHCons.Aux[Repr, Head, HNil]  //ensure it's length
  ): Head = gen.to(input).head
}



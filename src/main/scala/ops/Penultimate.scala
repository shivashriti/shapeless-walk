package ops

import shapeless.ops.hlist
import shapeless.{Generic, HList}

/**
  * lemma pattern
  * chain of dependently typed operations captured in single type class
  */
trait Penultimate[L] {
  type Out
  def apply(l: L): Out
}

object Penultimate {
  type Aux[L, O] = Penultimate[L] {type Out = O}

  def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p

  // penultimate instances
  implicit def hlistPenultimate[L <: HList, M <: HList, O](
    implicit
    init: hlist.Init.Aux[L, M],   // preserve init M of HList L
    last: hlist.Last.Aux[M, O]    // apply last on preserved M to get penultimate
    ): Penultimate.Aux[L, O] =
    new Penultimate[L] {
      type Out = O
      def apply(l: L): O = last.apply(init.apply(l))
    }

  implicit class PenultimateOps[A](a: A){
    def penultimate(implicit instance: Penultimate[A]): instance.Out = instance.apply(a)
  }

  implicit def genericPenultimate[A, Repr, O](
    implicit
    gen: Generic.Aux[A, Repr],
    pen: Penultimate.Aux[Repr,O]
    ): Penultimate.Aux[A, O] =
    new Penultimate[A] {
      type Out = O
      def apply(a: A): O = pen.apply(gen.to(a))
    }
}
package ops

import shapeless.ops.hlist
import shapeless.{HList, HNil, LabelledGeneric, Lazy, ::}
import cats.Monoid
import cats.instances.all._
import shapeless.labelled.{FieldType, field}

/**
  * Migration of case classes
  * piece of pure Cumberbatch
  */
trait Migration[A,B] {
  def apply(a:A): B
}

object Migration {

  /**
    * 1. Get ARepr from A
    * 2. find out Common fields using intersection ops
    * 3. get Added fields using Diff ops
    * 4. get default values for Added fields using Monoid
    * 5. combine Common and Added fields using Prepend ops
    * 6. align as BRepr
    * 7. Get B from BRepr
    */
  implicit def genericMigration[A, B, ARepr <: HList, BRepr <: HList,
      Common <: HList, Added <: HList, Unaligned <: HList](
   implicit
   aGen: LabelledGeneric.Aux[A, ARepr],               // LabelledGeneric: for case class' field identification by name
   bGen: LabelledGeneric.Aux[B, BRepr],
   inter: hlist.Intersection.Aux[ARepr, BRepr, Common],
   diff: hlist.Diff.Aux[BRepr, Common, Added],        // required to get Added type but not needed at runtime
//   monoid: Monoid[Added],
   empty: Empty[Added],
   prepend: hlist.Prepend.Aux[Added, Common, Unaligned],
   align: hlist.Align[Unaligned, BRepr]
   ): Migration[A, B] = new Migration[A, B] {
    def apply(a: A): B = bGen.from(align.apply(prepend(/*monoid.empty*/ empty.value, inter.apply(aGen.to(a)))))
  }

  /**
    * Intersection enough for removing fields
    * Align enough for reordering fields
    * Adding fields requires adding default values, that leads us to use cats' Monoid
    */

  /*
  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] = new Monoid[A]{
    def empty = zero
    def combine(x: A, y: A): A = add(x, y)
  }

  implicit val hnilMonoid: Monoid[HNil] = createMonoid[HNil](HNil)((x, y) => HNil)

  implicit def hlistMonoid[K <: Symbol, H, T <: HList](
    implicit
    hMonoid: Lazy[Monoid[H]],
    tMonoid: Monoid[T]
    ): Monoid[FieldType[K, H] :: T] =
    createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty){
      (x, y) => field[K](hMonoid.value.combine(x.head, y.head)) :: tMonoid.combine(x.tail, y.tail)
    }
    */

  /**
    * Direct Monoid instances for HNil and HList doesn't work for adding fields in case class
    * Since we only need monoid's empty (not combine) so lets create Empty instances for HNil and HList
    */
  case class Empty[A](value: A)

  implicit def monoidEmpty[A](implicit monoid: Monoid[A]): Empty[A] = Empty(monoid.empty)

  implicit val hnilEmpty: Empty[HNil] = Empty(HNil)

  implicit def hlistEmpty[K <: Symbol, H, T <: HList](
    implicit
    hEmpty: Lazy[Empty[H]],
    tEmpty: Empty[T]
    ): Empty[FieldType[K, H] :: T] =
    Empty(field[K](hEmpty.value.value) :: tEmpty.value)
}

object Main extends App{
  import models.MigrationADTs._

  implicit class MigrationOps[A](a: A){
    def migrateTo[B](implicit migration: Migration[A, B]): B = migration.apply(a)
  }

  println(IceCreamV1("Sundae", 1, false).migrateTo[IceCreamV2a])
  println(IceCreamV1("Sundae", 1, false).migrateTo[IceCreamV2b])
  println(IceCreamV1("Sundae", 1, false).migrateTo[IceCreamV2c])
}
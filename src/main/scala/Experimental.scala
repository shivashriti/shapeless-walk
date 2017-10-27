
import models.SeparateEncoders._
import models.PairEncoder._
import models.CsvEncoder._
import models.ADTs._
import models.Second
import models.Second._
import shapeless.{::, HNil}

object Experimental extends App {
  val employees = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )
  //test separate encoding of types
  println(writeCsv(employees))
  println(writeCsv(iceCreams))

  //test pairEncoder
  println(writeCsv(employees zip iceCreams))

  //test CsvEncoder for product (single instance with generic)
  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly
  println(reprEncoder.encode("shiva" :: 1900 :: false :: HNil))

  val laptops = List(
    Laptop("Asus", 1, true),
    Laptop("MAC", 1, false)
  )
  println(writeCsv(laptops))

  //test Generic instance for product
  val attires = List(
    Attire("Shirt", true),
    Attire("Jeans", false)
  )
  println(writeCsv(attires))

  //test Generic instances for Coproduct
  val shapes: List[Shape] = List(   //mentioning List[Shape] is mandatory as scalac takes it as Product with Serializable with your_trait
    Rectangle(2.0, 3.0),
    Circle(1.0)
  )
  println(writeCsv(shapes))

  //test dependent type function Second
  val second = Second[String :: Boolean :: Int :: HNil]
  println(second("Shiva" :: true :: 344 :: HNil))

  //test chaining dependent function lastField
  println(lastField(Rect(Vec(1,2), Vec(3,4))))

  //test getWrappedValue
  println(getWrappedValue(Wrapper(10)))

  //test JsonEncoder
  {
    import models.JsonEncoder
    import models.JsonObjectEncoder._
    println(JsonEncoder[IceCream].encode(IceCream("sai", 90, false)))
    val shape: Shape = Circle(1.0)
//    println(JsonEncoder[Shape].encode(shape)) //TODO fix the implicit here
  }
}
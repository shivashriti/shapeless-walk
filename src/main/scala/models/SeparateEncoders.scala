package models

import models.ADTs._

object SeparateEncoders {

  //turn a value of type A into a row of CSV file
  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  //csvEncoder instance for custom data type
  implicit val employeeEncoder: CsvEncoder[Employee] = new CsvEncoder[Employee] {
    override def encode(value: Employee): List[String] = List(
      value.name, value.number.toString, if (value.manager) "yes" else "no"
    )
  }

  implicit val iceCreamEncoder: CsvEncoder[IceCream] =
    new CsvEncoder[IceCream] {
      override def encode(i: IceCream): List[String] =
        List(
          i.name,
          i.numCherries.toString,
          if(i.inCone) "yes" else "no"
        )
    }

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

}

package models

import models.SeparateEncoders._

object PairEncoder {

  implicit def pairEncoder[A, B](implicit aEncoder: CsvEncoder[A], bEncoder: CsvEncoder[B]): CsvEncoder[(A, B)] =
    new CsvEncoder[(A, B)] {
      def encode(value: (A, B)): List[String] = aEncoder.encode(value._1) ++ bEncoder.encode(value._2)
    }
}

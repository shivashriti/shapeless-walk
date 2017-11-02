name := "shapeless_guide"

version := "1.0"

scalaVersion := "2.12.3"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.typelevel" % "cats-core_2.12" % "1.0.0-RC1"
)


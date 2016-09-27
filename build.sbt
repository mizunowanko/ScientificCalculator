name := "ScientificCalculator"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.6",
  "org.specs2" %% "specs2-core" % "3.8.5" % "test",
  "org.specs2" %% "specs2-junit" % "3.8.5" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
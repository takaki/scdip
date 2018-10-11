name := "scdip"

version := "0.0.2016"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.1",
  //  "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.1",
  "org.scala-graph" %% "graph-core" % "1.12.5"
)

// https://mvnrepository.com/artifact/org.specs2/specs2-junit_2.11
libraryDependencies += "org.specs2" % "specs2-junit_2.12" % "4.3.4"

// https://mvnrepository.com/artifact/org.scalaz/scalaz-core_2.11
libraryDependencies += "org.scalaz" % "scalaz-core_2.12" % "7.2.26"

// https://mvnrepository.com/artifact/org.specs2/specs2-core_2.11
libraryDependencies += "org.specs2" % "specs2-core_2.12" % "4.3.4" % "test"
// https://mvnrepository.com/artifact/org.specs2/specs2-matcher-extra_2.11
libraryDependencies += "org.specs2" % "specs2-matcher-extra_2.12" % "4.3.4" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")

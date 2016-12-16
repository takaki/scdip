name := "scdip"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  //  "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.1",
  "org.scala-graph" %% "graph-core" % "1.11.2"
)

// https://mvnrepository.com/artifact/org.specs2/specs2-junit_2.11
libraryDependencies += "org.specs2" % "specs2-junit_2.11" % "3.8.5"

// https://mvnrepository.com/artifact/org.scalaz/scalaz-core_2.11
libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.2.7"

// https://mvnrepository.com/artifact/org.specs2/specs2-core_2.11
libraryDependencies += "org.specs2" % "specs2-core_2.11" % "3.8.6" % "test"
// https://mvnrepository.com/artifact/org.specs2/specs2-matcher-extra_2.11
libraryDependencies += "org.specs2" % "specs2-matcher-extra_2.11" % "3.8.6" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")

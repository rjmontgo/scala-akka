name := "Akka"

version := "1.0"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)
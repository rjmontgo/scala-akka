name := "Http"

version := "1.0"

scalaVersion := "2.13.1"


libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.3",
  "com.typesafe.akka" %% "akka-http"   % "10.1.11",
  "com.typesafe.akka" %% "akka-stream" % "2.6.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3")
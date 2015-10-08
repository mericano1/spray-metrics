import Versions._

version := "0.1.3.3"

organization := "info.devinprogress"

scalaVersion := "2.11.6"

scalacOptions := Seq("-unchecked", "-feature", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion % "provided",
  "io.spray" %% "spray-httpx" % sprayVersion,
  "io.spray" %% "spray-routing" % sprayVersion,
  "io.dropwizard.metrics" % "metrics-core" % "3.1.2",
  "io.spray" %% "spray-testkit" % sprayVersion % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

bintrayPackageLabels := Seq("spray.io", "metrics", "codahale")
name := "my-akka-project"

organization := "local.my-org"

version := "0.1.0"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0-M4"

libraryDependencies += "com.typesafe.akka" % "akka-actor-migration" % "2.0-M4"

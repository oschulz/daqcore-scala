name := "daqcore-scala"

organization := "de.tu-dortmund"

version := "0.1.0"

scalaVersion := "2.9.1"

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.1")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-P:continuations:enable")

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"


resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "TU-Do Physik E4 Releases" at "http://maven.e4.physik.uni-dortmund.de/maven2/releases/"

resolvers += "TU-Do Physik E4 Snapshots" at "http://maven.e4.physik.uni-dortmund.de/maven2/snapshots/"


libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0-RC2"

libraryDependencies += "com.typesafe.akka" % "akka-actor-migration" % "2.0-RC2"

libraryDependencies += "com.typesafe.akka" % "akka-remote" % "2.0-RC2"

libraryDependencies += "com.typesafe.akka" % "akka-agent" % "2.0-RC2"

libraryDependencies += "com.typesafe.akka" % "akka-transactor" % "2.0-RC2"

libraryDependencies += "com.typesafe.akka" % "akka-slf4j" % "2.0-RC2"


libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.7.1" % "test"


libraryDependencies += "net.databinder" % "dispatch-http_2.9.1" % "0.8.7" % "compile"

libraryDependencies += "net.databinder" % "dispatch-http-json_2.9.1" % "0.8.7" % "compile"


libraryDependencies +=  "ch.qos.logback" % "logback-core" % "1.0.0"

libraryDependencies +=  "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime" intransitive()

libraryDependencies +=  "org.slf4j" % "log4j-over-slf4j" % "1.6.4" % "runtime" intransitive()


libraryDependencies += "jfree" % "jfreechart" % "1.0.13"


libraryDependencies += "com.thoughtworks.xstream" % "xstream" % "1.4.2"

libraryDependencies += "org.codehaus.jettison" % "jettison" % "1.0.1"


libraryDependencies += "colt" % "colt" % "1.2.0"

libraryDependencies += "org.acplt" % "remotetea-oncrpc" % "1.0.7"

libraryDependencies += "org.snmp4j" % "snmp4j" % "1.11.3"

libraryDependencies += "net.percederberg.mibble" % "mibble-parser" % "2.9.2"

libraryDependencies += "net.percederberg.mibble" % "mibble-mibs" % "2.9.2" % "runtime"

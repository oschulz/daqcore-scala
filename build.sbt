name := "daqcore-scala"

organization := "daqcore"

version := "0.1.0"

packageArchetype.java_application

scalaVersion := "2.11.6"

compileOrder := CompileOrder.JavaThenScala

autoCompilerPlugins := true

Classpaths.compilerPluginConfig

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies <+= (scalaVersion) { sv => "org.scala-lang" % "scala-compiler" % sv % "runtime" }

libraryDependencies <+= (scalaVersion) { sv => "org.scala-lang" % "scala-reflect" % sv % "runtime" }

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.2"

libraryDependencies += "jline" % "jline" % "2.12" % "runtime"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "DAQCorE Releases" at "http://daqcore.github.io/repo/releases/"


libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.9"

libraryDependencies += "com.typesafe.akka" %% "akka-dataflow" % "2.3.9"

libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.3.9"


libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "com.typesafe.play" %% "play" % "2.3.4"


libraryDependencies +=  "ch.qos.logback" % "logback-core" % "1.0.13"

libraryDependencies +=  "ch.qos.logback" % "logback-classic" % "1.0.13" % "runtime" intransitive()

libraryDependencies +=  "org.slf4j" % "log4j-over-slf4j" % "1.7.5" % "runtime" intransitive()


libraryDependencies += "jfree" % "jfreechart" % "1.0.13"


libraryDependencies += "com.thoughtworks.xstream" % "xstream" % "1.4.5"

libraryDependencies += "org.codehaus.jettison" % "jettison" % "1.3.3"


libraryDependencies += "com.nativelibs4java" % "bridj" % "0.6.2"

libraryDependencies += "colt" % "colt" % "1.2.0"

libraryDependencies += "org.acplt" % "remotetea-oncrpc" % "1.0.7"

libraryDependencies += "org.snmp4j" % "snmp4j" % "1.11.3"

libraryDependencies += "net.percederberg.mibble" % "mibble-parser" % "2.9.2"

libraryDependencies += "net.percederberg.mibble" % "mibble-mibs" % "2.9.2" % "runtime"

libraryDependencies += "net.wimpi" % "jamod" % "1.2"

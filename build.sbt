name := "daqcore-scala"

organization := "de.tu-dortmund"

version := "0.1.0"

scalaVersion := "2.10.3"

compileOrder := CompileOrder.JavaThenScala

autoCompilerPlugins := true

libraryDependencies <+= scalaVersion { sv => compilerPlugin("org.scala-lang.plugins" % "continuations" % sv) }

Classpaths.compilerPluginConfig

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-P:continuations:enable")

libraryDependencies <+= (scalaVersion) { sv => "org.scala-lang" % "scala-swing" % sv }

libraryDependencies <+= (scalaVersion) { sv => "org.scala-lang" % "jline" % sv % "runtime" }

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "DAQCorE Releases" at "http://daqcore.github.io/repo/releases/"


libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.2.3"

libraryDependencies += "com.typesafe.akka" %% "akka-dataflow" % "2.2.3"

libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.2.3"


libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "com.typesafe.play" %% "play" % "2.2.1"


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

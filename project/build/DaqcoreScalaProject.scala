import sbt._
import Process._

class DaqcoreScalaProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  override def parallelExecution = true

  val publishTo = Resolver.file("maven-local", Path.userHome / ".m2" / "repository" asFile)
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  val tuDoE4Releases = "TU-Do Physik E4 Releases" at "http://maven.e4.physik.uni-dortmund.de/maven2/releases/"
  val tuDoE4Snapshots = "TU-Do Physik E4 Releases" at "http://maven.e4.physik.uni-dortmund.de/maven2/snapshots/"

  val snapshotsRepo = ScalaToolsSnapshots

  val jBossRepo = "jBoss" at "http://repository.jboss.org/nexus/content/groups/public/"
  val multiverseRepo = "Multiverse Releases" at "http://multiverse.googlecode.com/svn/maven-repository/releases/"
  val guiceyfruitRepo = "GuiceyFruit" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"
  val akkaRepo = "Akka Maven Repository" at "http://scalablesolutions.se/akka/repository"

  val scala_continuations = compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.8.1")

  val scala_swing = "org.scala-lang" % "scala-swing" % "2.8.1"

  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"

  val lift_common = "net.liftweb" % "lift-common_2.8.0" % "2.2-M1"
  val lift_util = "net.liftweb" % "lift-util_2.8.0" % "2.2-M1"

  val akka_actor = "se.scalablesolutions.akka" % "akka-actor" % "1.0-RC1"
  val akka_remote = "se.scalablesolutions.akka" % "akka-remote" % "1.0-RC1"
  val akka_stm = "se.scalablesolutions.akka" % "akka-stm" % "1.0-RC1"

  val mina_ore = "org.apache.mina" % "mina-core" % "2.0.0-RC1"
  val mina_serial = "org.apache.mina" % "mina-transport-serial" % "2.0.0-RC1"

  val netty = "org.jboss.netty" % "netty" % "3.2.1.Final" % "compile"

  val jfreechart = "jfree" % "jfreechart" % "1.0.13"
  val jcommon = "jfree" % "jcommon" % "1.0.15"

  val xstream = "com.thoughtworks.xstream" % "xstream" % "1.3.1"
  val jettison = "org.codehaus.jettison" % "jettison" % "1.0.1"

  val colt = "colt" % "colt" % "1.2.0"
  val remotetea_oncrpc = "org.acplt" % "remotetea-oncrpc" % "1.0.7"

  def javaSrcDir = "src" / "main" / "java"

  def jrpcgenTask(pkgName: String, client: String, server: String, xdrName: String) = {
    def xdrSrcDir = "src" / "main" / "xdr"
    def xdrFile = xdrSrcDir / xdrName
    val targetDir = pkgName.split('.').foldLeft (javaSrcDir) ((path, part) => path / part)

    val cleanSrc = cleanTask(targetDir) named("clean-oncrpc-" + pkgName)
    execTask {
      FileUtilities.createDirectory(targetDir, log)
      <x>jrpcgen -ser -nobackup -d {targetDir.absolutePath} -p {pkgName} -c {client} -s {server} {xdrFile}</x>
    } dependsOn(cleanSrc)
  }

  lazy val rpcgen_vxi11core = jrpcgenTask("daqcore.oncrpc.vxi11core", "Client", "Server", "vxi11core.x") describedAs("Generate vxi11core classes and stubs.")
  lazy val rpcgen = task {None} dependsOn(rpcgen_vxi11core)
  
  override def compileOptions = super.compileOptions ++
    Seq(Unchecked) ++ // Enable unchecked warnings
    compileOptions("-P:continuations:enable") // Enable continuations plugin
  
  // override def compileOrder = CompileOrder.JavaThenScala

  //// Scaladoc workaround for sbt + scala-2.8
  //private val prjDocTitle = projectName + " " + projectVersion + " API Docs"
  //override def documentOptions = Seq(CompoundDocOption("-doc-title", prjDocTitle))
}

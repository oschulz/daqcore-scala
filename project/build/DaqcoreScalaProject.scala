import sbt._
import Process._

class DaqcoreScalaProject(info: ProjectInfo) extends DefaultProject(info) {
  override def parallelExecution = true

  val onrpc = "org.acplt" % "remotetea" % "1.0.7"

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
  
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
  // override def compileOrder = CompileOrder.JavaThenScala

  // Scaladoc workaround for sbt + scala-2.8
  private val prjDocTitle = projectName + " " + projectVersion + " API Docs"
  override def documentOptions = Seq(CompoundDocOption("-doc-title", prjDocTitle))
}

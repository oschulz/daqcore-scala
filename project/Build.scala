import sbt._
import Keys._

object MyBuild extends Build {

  // copy-dependencies by Fred Dubois

  lazy val copyDependencies = TaskKey[Unit]("copy-dependencies")

  def copyDepTask = copyDependencies <<= (update, crossTarget, scalaVersion) map {
    (updateReport, out, scalaVer) =>
    updateReport.select(configuration = Set("runtime", "plugin")) foreach { srcPath =>
      val destPath = out / "lib" / srcPath.getName
      IO.copyFile(srcPath, destPath, preserveLastModified=true)
    }
  }


  lazy val root = Project(
    "root",
    file("."),
    settings = Defaults.defaultSettings ++ Seq(
      copyDepTask,
      console <<= Defaults.consoleTask(fullClasspath in Runtime, console)
    )
  )
}

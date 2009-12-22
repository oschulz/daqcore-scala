import sbt._

class DaqcoreScala(info: ProjectInfo) extends DefaultProject(info) {
  override def parallelExecution = true
}

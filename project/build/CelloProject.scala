import sbt._

class CelloProject(info: ProjectInfo) extends DefaultProject(info) {
  
  val scalatest = "org.scalatest" % "scalatest" % "1.3"
  
}
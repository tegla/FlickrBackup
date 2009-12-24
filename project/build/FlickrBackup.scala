import sbt._

class FlickrBackupProject(info: ProjectInfo) extends DefaultProject(info)
{
	val scalaToolsRepo = "Scala-Tools Maven Repository" at
        		"http://scala-tools.org/repo-releases"
	val scalatest = "org.scala-tools.testing" % "scalatest" % "0.9.5"
}


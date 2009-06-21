import sbt._

class ScalaxProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"
	
  override def testScalaSourcePath = "tests"
  override def testResourcesPath = "tests-resources"


  val slf4j = "org.slf4j" % "slf4j-api" % "1.5.0"

  //override def managedStyle = ManagedStyle.Maven
  override def crossScalaVersions = Set("2.7.2", "2.7.3", "2.7.4", "2.7.5")



  //Test task
  override def testAction = runTask(Some("ScalaxTests"), testClasspath)
}



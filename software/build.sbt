name := "REACT"

version := "0.1-SNAPSHOT"

organization := "edu.mit.csail.pac"


//configuration for LMS
//scalaOrganization := "org.scala-lang.virtualized"

//scalaVersion := "2.10.2-RC1"
scalaVersion := "2.10.3"

//scalacOptions += "-Yvirtualize"

scalacOptions in Compile ++= Seq("-unchecked", "-deprecation")

//libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

// needed for scala.tools, which is apparently not included in sbt's built in version
//libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % scalaVersion.value

//libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-actors" % scalaVersion.value // for ScalaTest

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"
    
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.2.1"

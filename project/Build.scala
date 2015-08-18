import sbt._
import Keys._

object BuildSettings {
  val baseDir = System.getProperty("user.dir")
  val buildVersion = "0.1.0-SNAPSHOT"
  val buildScalaVersion = System.getProperty("scala.version", "2.11.7")
  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    resolvers += "Local ROS Java maven repo" at "file://"+baseDir+"/rosjava-maven",
    resolvers += "ROS Java maven repo" at "https://github.com/rosjava/rosjava_mvn_repo/raw/master",
    resolvers += "dzufferey maven repo" at "https://github.com/dzufferey/my_mvn_repo/raw/master/repository",
    unmanagedBase := new java.io.File(baseDir + "/lib"),
    parallelExecution in Test := false,
    publishMavenStyle := true,
    scalacOptions in Compile ++= Seq(
        "-unchecked",
        "-deprecation",
        "-feature",
        "-language:implicitConversions",
        "-Xmax-classfile-name", "110"//,
    //    "-Ymacro-debug-lite"
    //    "-Xlog-implicits"
    //    "-Xlog-implicit-conversions"
    )
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root = (project in file(".")).aggregate(compiler, verifier, examples)

  lazy val compiler: Project = Project(
    "react-compiler",
    file("compiler"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          "org.scalatest" %% "scalatest" % "2.2.4" % "test",
          /* ROS dependencies */
          "org.ros.rosjava_bootstrap" % "message_generation" % "0.2.1",
          "org.ros.rosjava_core" % "rosjava" % "0.2.1",
          "org.ros.rosjava_messages" % "std_msgs"  % "0.5.9",
          "org.ros.rosjava_messages" % "geometry_msgs" % "1.11.7",
          "org.ros.rosjava_messages" % "sensor_msgs" % "1.11.7",
          "org.ros.rosjava_messages" % "nav_msgs" % "1.11.7",
          "org.ros.rosjava_messages" % "gazebo_msgs" % "2.4.7",
          /* Other dependencies */
          "org.scream3r" % "jssc" % "2.8.0",
          "io.silverspoon" % "bulldog-board-beagleboneblack" % "0.1.2",
          "io.silverspoon" % "bulldog-devices" % "0.1.2",
          "io.github.dzufferey" %% "misc-scala-utils" % "0.1-SNAPSHOT"
      )
    )
  )

  lazy val verifier: Project = Project(
    "react-verifier",
    file("verifier"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
          "org.scalatest" %% "scalatest" % "2.2.4" % "test",
          "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
          /* ROS dependencies */
          "org.ros.rosjava_core" % "rosjava" % "0.2.1",
          /* Other dependencies */
          "org.apache.commons" % "commons-lang3" % "3.2.1",
          "commons-codec" % "commons-codec" % "1.10",
          "net.automatalib" % "automata-parent" % "0.5.2",
          "net.automatalib" % "automata-api" % "0.5.2",
          "net.automatalib" % "automata-core" % "0.5.2",
          "net.automatalib" % "automata-util" % "0.5.2",
          "net.automatalib" % "automata-incremental" % "0.5.2",
          "net.automatalib" % "automata-misc-algorithms" % "0.5.2",
          "net.automatalib" % "automata-brics" % "0.5.2",
          "net.automatalib" % "automata-commons-util" % "0.5.2",
          "net.automatalib" % "automata-commons-smartcollections" % "0.5.2",
          "net.automatalib" % "automata-commons-dotutil" % "0.5.2",
          "io.github.dzufferey" %% "misc-scala-utils" % "0.1-SNAPSHOT",
          "io.github.dzufferey" %% "scala-smtlib-interface" % "0.1-SNAPSHOT"
      )
    )
  ).dependsOn(compiler)

  lazy val examples: Project = Project(
    "react-examples",
    file("examples"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        /* ROS dependencies */
        "org.ros.rosjava_core" % "rosjava" % "0.2.1",
        "org.ros.rosjava_messages" % "std_msgs" % "0.5.9",
        "org.ros.rosjava_messages" % "geometry_msgs" % "1.11.7",
        "org.ros.rosjava_messages" % "sensor_msgs" % "1.11.7",
        "org.ros.rosjava_messages" % "turtlesim" % "0.5.2",
        "io.github.dzufferey" %% "scala-arg" % "0.1-SNAPSHOT",
        "io.github.dzufferey" %% "misc-scala-utils" % "0.1-SNAPSHOT"
      )
    )
  ).dependsOn(verifier)

}


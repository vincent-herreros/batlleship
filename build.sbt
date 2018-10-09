import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "batlleship",
    libraryDependencies ++= Seq(
    	scalaTest % Test,
    	"com.github.tototoshi" %% "scala-csv" % "1.3.5"
    )
  )

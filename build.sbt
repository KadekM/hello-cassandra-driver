import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion := "2.13.5"

lazy val root = (project in file("."))
  .settings(
    name := "Hello Cassandra Driver",
    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-core" % "1.11.7",
      "org.scodec" %% "scodec-bits" % "1.1.24",
      "dev.zio" %% "zio-nio" % "1.0.0-RC10",
      "dev.zio" %% "zio-logging" % "0.5.8",
    )
  )


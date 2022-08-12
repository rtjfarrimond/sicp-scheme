ThisBuild / scalaVersion     := "3.1.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.rtjfarrimond"
ThisBuild / organizationName := "rtjfarrimond"

val fs2Version = "3.2.11"

lazy val root = (project in file("."))
  .settings(
    name := "sicp-scheme",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "co.fs2"        %% "fs2-core"  % fs2Version,
      "co.fs2"        %% "fs2-io"    % fs2Version,
      "org.scalameta" %% "munit"     % "0.7.29" % Test
    )
  )

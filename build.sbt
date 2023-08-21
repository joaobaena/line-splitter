ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.3"

val catsVersion = "3.5.1"
val fs2Version = "3.8.0"
lazy val root = (project in file("."))
  .settings(
    name := "line-splitter",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"        % catsVersion,
      "org.typelevel" %% "cats-effect-kernel" % catsVersion,
      "org.typelevel" %% "cats-effect-std"    % catsVersion,
      "co.fs2"        %% "fs2-core"           % fs2Version
    )
  )

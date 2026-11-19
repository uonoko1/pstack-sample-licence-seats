ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "licence-seats",
    scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all", "-Werror"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test,
    Test / parallelExecution := false
  )

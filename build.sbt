val scala2Version = "2.13.8"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fetch-covid-status",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala2Version,
    libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.30.0",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test",
    libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.2.1",
    libraryDependencies += "io.lemonlabs" %% "scala-uri" % "3.6.0"
  )

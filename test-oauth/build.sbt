name := "test_oauth"

version := "1.0"

scalaVersion := "2.9.1"

parallelExecution in Test := false

libraryDependencies +=
       "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


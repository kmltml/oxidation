name := "oxidation"

version := "1.0"

scalaVersion := "2.12.1"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies ++=
  "com.lihaoyi" %% "fastparse" % "0.4.2" ::
  "com.lihaoyi" %% "utest" % "0.4.5" % "test" ::
  "org.typelevel" %% "cats" % "0.9.0" ::
  Nil

testFrameworks += new TestFramework("utest.runner.Framework")

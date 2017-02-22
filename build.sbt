name := "oxidation"

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies ++=
  "com.lihaoyi" %% "fastparse" % "0.4.1" ::
  "com.lihaoyi" %% "utest" % "0.4.3" % "test" ::
  Nil

testFrameworks += new TestFramework("utest.runner.Framework")
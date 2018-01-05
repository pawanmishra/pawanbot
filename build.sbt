name := "PawanBot"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test

lazy val root = (project in file(".")).
  settings(
    assemblyJarName in assembly := "ScalatronBot.jar",
    target in assembly := file("/Users/mishrapaw/ScalatronPawan/dist/bots/pawan/")
  )
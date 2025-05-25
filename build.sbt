name := "cats-effect"

version := "0.1"

scalaVersion := "3.5.2"

val catsVersion = "2.13.0"
val catsEffectVersion = "3.6.1"
val scalaTestVersion = "3.2.18"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)

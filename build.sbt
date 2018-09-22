resolvers += Resolver.sonatypeRepo("releases")

val Scala2_11 = "2.11.12"
val Scala2_12 = "2.12.6"

val versionSetting = Seq(
  organization := "com.todesking",
  version := "0.0.1-SNAPSHOT"
)

val commonSettings = versionSetting ++ Seq(
  scalaVersion := Scala2_11,
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Xfuture",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard"
  ),
  scalacOptions in (Compile, console) ~= {_.filterNot(_ == "-Ywarn-unused-import")},
  scalacOptions in (Test, console) := { (scalacOptions in (Compile, console)).value }
)

val testSettings = commonSettings ++ Seq(
  scalacOptions -= "-Ywarn-value-discard"
)

val coreCross = Seq(
  scalaVersion := Scala2_11,
  crossScalaVersions := Seq(Scala2_11, Scala2_12)
)
lazy val core = project
  .settings(commonSettings)
  .settings(coreCross)
lazy val test = project
  .dependsOn(core)
  .settings(testSettings)
  .settings(coreCross)
lazy val example = project
  .dependsOn(core)
  .settings(commonSettings)
  .settings(coreCross)

val sparkCross = Seq(
  scalaVersion := Scala2_11,
  crossScalaVersions := Seq(Scala2_11)
)
lazy val spark = project
  .dependsOn(core)
  .settings(commonSettings)
  .settings(sparkCross)
lazy val sparkTest = project.in(file("spark-test"))
  .dependsOn(spark)
  .dependsOn(test)
  .settings(testSettings)
  .settings(sparkCross)

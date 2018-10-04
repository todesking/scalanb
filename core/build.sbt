name := "scalanb-core"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9"
dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-annotations" % "2.8.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

sourceGenerators in Compile += Def.task {
  val file = (sourceManaged in Compile).value / "impl" / "cache" / "DepArgTupleInstances.scala"
  def instance(n: Int) = {
    val range = (1 to n)
    val tparamsIn = range.map { i => s"A$i, B$i" }.mkString(", ")
    val tparamsX = range.map { i => s"A$i" }.mkString("(", ", ", ")")
    val tparamsOut = range.map { i => s"B$i" }.mkString("(", ", ", ")")
    val evs = range.map { i => s"ev$i: Dependable[A$i, B$i]" }.mkString(", ")
    s"""implicit def fromTuple$n[$tparamsIn]
  (x: $tparamsX)
  (implicit $evs)
  : DepArg[$tparamsOut] = {
${
  range.map { i =>
    s"  val a$i: Dep[B$i] = ev$i(x._$i)"
  }.mkString("\n")
}
  val ids = Seq(${
    range.map { i => s"a$i.id" }.mkString(", ")
  })
  new DepArg(ids, ${
    range.map { i => s"a$i.unwrapUNSAFE" }.mkString(", ")
  })
}
"""
  }
  val instances = (2 to 22).map { n => instance(n) }

  val src = s"""package com.todesking.scalanb.impl.cache
import com.todesking.scalanb.cache.Dep
import com.todesking.scalanb.cache.DepArg
import com.todesking.scalanb.cache.Dependable

import scala.language.implicitConversions

trait DepArgTupleInstances {
${instances.mkString("\n")}
}
"""
  IO.write(file,src)
  Seq(file)
}

scalariformSettings(true)


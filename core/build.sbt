name := "scalanb-core"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9"
dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-annotations" % "2.8.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

// For REPL classpath problem
fork in run := true
connectInput := true

scalariformSettings(true)


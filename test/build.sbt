addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// scalacOptions += "-Ymacro-debug-lite"
scalacOptions += "-Yrangepos"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"

dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-annotations" % "2.8.9"

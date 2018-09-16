addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// scalacOptions += "-Ymacro-debug-lite"
scalacOptions += "-Yrangepos"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// scalacOptions += "-Ymacro-debug-lite"
scalacOptions += "-Yrangepos"

dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-annotations" % "2.8.9"

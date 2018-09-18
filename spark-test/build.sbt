addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

val jacksonVersion = "2.6.7.1"
val sparkVersion = "2.3.1"

// scalacOptions += "-Ymacro-debug-lite"
scalacOptions += "-Yrangepos"

libraryDependencies += "org.apache.spark" %% "spark-sql" % sparkVersion % "provided"
libraryDependencies += "org.apache.spark" %% "spark-hive" % sparkVersion % "provided"

dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion

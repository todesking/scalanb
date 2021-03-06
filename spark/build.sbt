name := "scalanb-spark"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

val sparkVersion ="[2.0.0,3.0.0["
libraryDependencies += "org.apache.spark" %% "spark-sql" % sparkVersion % "provided"
libraryDependencies += "org.apache.spark" %% "spark-mllib" % sparkVersion % "provided"

dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-annotations" % "2.8.9"

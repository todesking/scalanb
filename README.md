# scalanb: Scala notebook

Status: PoC

## Installation

Scalanb is not published yet.

```sbt
// In build.sbt

// To use batch notebook, you need macro paradise plugin and additional compiler options.
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
```

## Batch Notebook

1. Setup dependencies, compiler plugin, scalac options in `build.sbt`
2. Create notebook class with `@Notebook` annotation
3. Run notebook (`main` method is automatically generated)
4. `.ipynb` is saved in `~/.scalanb/hist`(default)

```scala
import com.todesking.{scalanb => nb}

@nb.Notebook
class MyNotebook {
  nb.markdown("# Example of scalanb")
  // add more code here
}
```

and

```shellsession
$ sbt 'runMain MyNotebook'
```

See [Example1.scala](example/src/main/scala/Example1.scala) and [its output](example/output/Example1.ipynb)

To specify history location, use `--out` option.

```shellsession
$ sbt 'runMain MyNotebook --out=file:path=./hist/'
```

## Spark Batch Notebook

Use `spark.Notebook` annotation

```scala
import com.todesking.{scalanb => nb}

@nb.spark.Notebook
class MyNotebook {
  // spark session available here
  val df = spark.read.csv("...")

  // Show dataframe as HTML tables via `nb.show` method
  df.nb.show(10)
}
```

```shellsession
$ sbt assembly # Make fatjar
$ spark-submit --class MyNotebook myapp.jar
```

## Example: Save history to HDFS

```shellsession
$ sbt 'runMain MyNotebook --out=hdfs:path=/tmp/hist/'
```


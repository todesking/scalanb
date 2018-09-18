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

## Save history to HDFS

Requirement: `scalanb-spark`

```shellsession
$ sbt 'runMain MyNotebook --out=hdfs:path=/tmp/hist/'
```

### Execution log

When `--log` option enabled, realtime log available.

```shellsession
$ sbt 'runMain MyNotebook --log'
```

```
# .scalanb/hist/{TIME}_{NOTE_NAME}.log
[2018-08-21 21:46:48] > nb.setShowTimeMillis(100)

[2018-08-21 21:46:48] > nb.markdown("# Scalanb Example")

[2018-08-21 21:46:48] > val a = 1

[2018-08-21 21:46:48] > val b = 2

[2018-08-21 21:46:48] > a
[2018-08-21 21:46:48] => 1

[2018-08-21 21:46:48] > println(s"a = $a")
[2018-08-21 21:46:48] stdout: a = 1
```

### Caching

```scala
import com.todesking.{scalanb => nb}
@nb.Notebook
class BigData {
  val cp = nb.checkpoint

  val rawLog = cp.nocache { loadData("data/raw.csv") }
  val count = cp.cache(rawLog) { rawLog => rawLog.count() }
  cp.unwrap(count) { count =>
    println(s"count = $count")
  }

  val userId = 10
  val theUsersLogs = cp.cache((rawLog, userId)) { case (rawLog, userId) =>
    rawLog.where('user_id === userId)
  }

  cp.unwrap(theUsersLogs) { theUsersLogs =>
    theUsersLogs.count()
    theUsersLogs.show()
  }
}
```

Cache is based on value's ID.
ID calculated from
* val name
* AST
* Dependent values
* Runtime value(if supported)

```scala
// ID: rawLog-{ loadData("data/raw.csv") }
val rawLog = cp.nocache { loadData("data/raw.csv") }

// ID: count-{ rawLog => rawLog.count() }(rawLog-{ loadData("data/raw.csv") })
val count = cp.cache(rawLog) { rawLog => rawLog.count() }

// Primitive values could be dependent value.
// ID: lit:10
val userId = 10

// ID: theUsersLogs-{ case (rawLog, userId) => rawLog.where('user_id === userId) }((rawLog-{ loadData("data/raw.csv") }, lit:10))
val theUsersLogs = cp.cache((rawLog, userId)) { case (rawLog, userId) =>
  rawLog.where('user_id === userId)
}
```

Cache location could specified by `--cache` option. Default is `~/.scalanb/cache/`

```
--cache=file:path=/path/to/cache
--cache=hdfs:path=/path/to/cache # requires scalanb-spark
```

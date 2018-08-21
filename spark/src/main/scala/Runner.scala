package com.todesking.scalanb.spark

import com.todesking.scalanb
import scalanb.Builder
import scalanb.Format

import org.apache.spark.sql.SparkSession

object Runner {
  type TargetType = {
    def scalanb__run(spark: SparkSession)(implicit builder: Builder): Unit
  }

  def runBatch(args: Array[String], target: TargetType, notebookName: String): Unit = {
    import scala.language.reflectiveCalls

    val logName = scalanb.Runner.newLogName(notebookName)
    val (parsedArgs, _) = scalanb.Runner.parseArgs(args)

    val spark = SparkSession.builder()
      .appName(s"Notebook:$notebookName").getOrCreate()

    val out = parsedArgs.out

    val logWriter =
      if (parsedArgs.useLog) Some(new java.io.PrintWriter(out.openLog(logName)))
      else None

    val ipynbBuilder = new Builder.Ipynb()
    val builder = logWriter.fold[Builder](ipynbBuilder) { w =>
      new Builder.Multiplex(Seq(ipynbBuilder, new Builder.Log(w)))
    }

    try {
      scalanb.Runner.run(builder) { builder =>
        val _ = try {
          target.scalanb__run(spark)(builder)
        } catch {
          case e: java.lang.reflect.InvocationTargetException =>
            throw e.getCause
        }
      }
    } finally {
      try {
        val filePath = out.notebook(logName, ipynbBuilder.build())
        println(s"scalanb: Notebook log saved to ${filePath}")
      } finally {
        logWriter.foreach(_.close())
      }
    }
  }
}

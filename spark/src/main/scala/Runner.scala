package com.todesking.scalanb.spark

import com.todesking.scalanb
import scalanb.Builder
import scalanb.Format

import org.apache.spark.sql.SparkSession

object Runner {
  type TargetType = {
    def scalanb__run(spark: SparkSession)(implicit builder: Builder): Unit
  }

  def runBatch(args: Array[String], notebookName: String, target: TargetType): Unit = {
    import scala.language.reflectiveCalls
    val spark = SparkSession.builder()
      .appName(s"Notebook:$notebookName").getOrCreate()
    scalanb.Runner.runBatch(args, notebookName) { builder =>
      try {
        target.scalanb__run(spark)(builder)
      } catch {
        case e: java.lang.reflect.InvocationTargetException =>
          throw e.getCause
      }
    }
  }
}

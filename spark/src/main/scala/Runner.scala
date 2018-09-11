package com.todesking.scalanb.spark

import com.todesking.scalanb
import scalanb.Builder
import scalanb.Format

import org.apache.spark.sql.SparkSession

object Runner {
  type TargetType = {
    def scalanb__run(spark: SparkSession)(implicit builder: Builder): Unit
  }

  def runBatch(args: Array[String], notebookName: String, src: String)(invoke: (Builder, SparkSession) => Unit): Unit = {
    val spark = SparkSession.builder()
      .appName(s"Notebook:$notebookName").getOrCreate()
    scalanb.Runner.runBatch(args, notebookName, src) { builder =>
      invoke(builder, spark)
    }
  }
}

package com.todesking.scalanb.spark

import com.todesking.scalanb
import scalanb.NotebookContext
import scalanb.Format

import org.apache.spark.sql.SparkSession

object Runner {
  def runBatch(args: Array[String], notebookName: String, src: String)(invoke: (NotebookContext, SparkSession) => Unit): Unit = {
    val spark = SparkSession.builder()
      .appName(s"Notebook:$notebookName").getOrCreate()
    scalanb.Runner.runBatch(args, notebookName, src) { ctx =>
      invoke(ctx, spark)
    }
  }
}

package com.todesking.scalanb.spark

import com.todesking.scalanb
import scalanb.NBContext
import scalanb.Format

import org.apache.spark.sql.SparkSession

object Runner {
  def runBatch(args: Array[String], nbName: String, nbClassName: String, src: String)(invoke: (NBContext, SparkSession) => Unit): Unit = {
    val spark = SparkSession.builder()
      .appName(s"Notebook:$nbName").getOrCreate()
    scalanb.Runner.runBatch(args, nbName, nbClassName, src) { ctx =>
      invoke(ctx, spark)
    }
  }
}

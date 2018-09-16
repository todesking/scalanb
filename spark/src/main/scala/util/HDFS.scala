package com.todesking.scalanb.spark.hdfs

import org.apache.hadoop.fs.FileSystem
import org.apache.hadoop.fs.Path
import org.apache.hadoop.conf.Configuration

object HDFS {
  lazy val fs = {
    val conf = new Configuration
    FileSystem.get(conf)
  }
  def exists(path: String): Boolean = {
    fs.exists(new Path(path))
  }
}

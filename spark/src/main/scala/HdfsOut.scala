package com.todesking.scalanb.spark

import com.todesking.scalanb.{ Out, OutFactory }
import com.todesking.scalanb.ipynb
import com.todesking.scalanb.spark.hdfs.HDFS

import org.apache.hadoop.fs.{ FileSystem, Path }
import org.apache.hadoop.conf.Configuration

import com.todesking.scalanb.spark.io.HdfsFileSystem

class HdfsOutFactory extends OutFactory {
  lazy val defaultPath = new Path(HDFS.fs.getHomeDirectory, ".scalanb/hist").toString
  override def name = "hdfs"
  override def newOut(args: Map[String, String]) = {
    val path = args.get("path") getOrElse defaultPath
    new HdfsOut(path)
  }
}

class HdfsOut(val path: String) extends Out {
  val fs = new HdfsFileSystem(path)
  override def prepare() = fs.prepare()

  override def newWriter(name: String) =
    fs.newWriter(name)
}

package com.todesking.scalanb.spark

import com.todesking.scalanb.{ Out, OutFactory }
import com.todesking.scalanb.ipynb
import com.todesking.scalanb.spark.hdfs.HDFS

import org.apache.hadoop.fs.{ FileSystem, Path }
import org.apache.hadoop.conf.Configuration

class HdfsOutFactory extends OutFactory {
  lazy val defaultPath = new Path(HDFS.fs.getHomeDirectory, ".scalanb/hist")
  override def name = "hdfs"
  override def newOut(args: Map[String, String]) = {
    val path = args.get("path").map(new Path(_)) getOrElse defaultPath
    new HdfsOut(HDFS.fs, path)
  }
}

class HdfsOut(val fs: FileSystem, val path: Path) extends Out {
  // TODO: check hdfs connectivity
  override def prepare() = {}

  override def newWriter(name: String) = {
    val stream = fs.create(new Path(path, name))
    val w = new java.io.OutputStreamWriter(stream)
    new java.io.Writer {
      override def write(cbuf: Array[Char], off: Int, len: Int) =
        w.write(cbuf, off, len)
      override def close() = w.close()
      override def flush() = {
        w.flush()
        stream.hflush()
      }
    }
  }
}

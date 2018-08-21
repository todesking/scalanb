package com.todesking.scalanb.spark

import com.todesking.scalanb.{ Out, OutFactory }
import com.todesking.scalanb.ipynb

import org.apache.hadoop.fs.{ FileSystem, Path }
import org.apache.hadoop.conf.Configuration

class HdfsOutFactory extends OutFactory {
  lazy val fs = {
    val conf = new Configuration
    FileSystem.get(conf)
  }
  lazy val defaultPath = new Path(fs.getHomeDirectory, ".scalanb/hist")
  override def name = "hdfs"
  override def newOut(args: Map[String, String]) = {
    val path = args.get("path").map(new Path(_)) getOrElse defaultPath
    new HdfsOut(fs, path)
  }
}

class HdfsOut(val fs: FileSystem, val path: Path) extends Out {
  // TODO: check hdfs connectivity
  override def prepare() = {}
  override def notebook(name: String, ast: ipynb.Notebook): String = {
    import scala.collection.JavaConverters._
    val src = ipynb.JsonMapping.toJson(ast, pretty = true)
    prepare()
    val filePath = new Path(path, s"$name.ipynb")
    val w = new java.io.PrintWriter(fs.create(filePath))
    try {
      w.print(src)
    } finally {
      w.close()
    }
    filePath.toString
  }
  override def openLog(name: String) = {
    val stream = fs.create(new Path(path, s"$name.log"))
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

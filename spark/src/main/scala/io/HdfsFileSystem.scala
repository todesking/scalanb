package com.todesking.scalanb.spark.io

import java.io.{ InputStream, OutputStream }
import java.io.{ BufferedInputStream, BufferedOutputStream }

import org.apache.hadoop.fs.Path

import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.io.FileSystemFactory
import com.todesking.scalanb.spark.hdfs.HDFS

class HdfsFileSystemFactory extends FileSystemFactory {
  lazy val defaultPath = new Path(HDFS.fs.getHomeDirectory, ".scalanb/hist").toString

  override val name = "hdfs"
  override def newFS(args: Map[String, String]) = {
    val path = args.get("path") getOrElse defaultPath
    new HdfsFileSystem(path)
  }
}

class HdfsFileSystem(val basePath: String) extends FileSystem {
  private[this] val fs = HDFS.fs
  private[this] val base = new Path(basePath)
  private[this] val protocolRe = """(\w+)://""".r
  private[this] def resolve(path: String) = new Path(base, path)

  override val protocol = basePath match {
    case `protocolRe`(p) => p
    case _ => "hdfs"
  }
  override def prepare() = {}

  override def newInputStream(path: String): InputStream =
    new BufferedInputStream(fs.open(resolve(path)))
  override def newOutputStream(path: String): OutputStream =
    new BufferedOutputStream(newHdfsOutputStream(path))
  def newHdfsOutputStream(path: String) =
    fs.create(resolve(path))

  override def newWriter(path: String) = {
    val os = newHdfsOutputStream(path)
    val w = new java.io.OutputStreamWriter(os)
    val writer =
      new java.io.Writer {
        override def write(cbuf: Array[Char], off: Int, len: Int) =
          w.write(cbuf, off, len)
        override def close() = w.close()
        override def flush() = {
          w.flush()
          os.hflush()
        }
      }
    new java.io.BufferedWriter(writer)
  }

  override def list(path: String): Seq[String] = {
    fs.listStatus(resolve(path)).map(_.getPath.getName)
  }
  override def exists(path: String): Boolean =
    fs.exists(resolve(path))
}

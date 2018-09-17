package com.todesking.scalanb.io

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.io.{ Reader, Writer }
import java.io.{ InputStream, OutputStream }
import java.io.{ BufferedInputStream, BufferedOutputStream }
import java.io.{ InputStreamReader, OutputStreamWriter }
import java.io.{ BufferedReader, BufferedWriter }
import java.util.stream.Collectors

import scala.collection.JavaConverters._

trait FileSystem {
  val protocol: String
  val basePath: String

  def prepare(): Unit

  def newWriter(path: String): BufferedWriter =
    new BufferedWriter(new OutputStreamWriter(newOutputStream(path)))
  def newReader(path: String): BufferedReader =
    new BufferedReader(new InputStreamReader(newInputStream(path)))

  def newInputStream(path: String): InputStream
  def newOutputStream(path: String): OutputStream

  def list(path: String): Seq[String]
  def exists(path: String): Boolean

  private[this] def withResource[A <: AutoCloseable, B](resource: A)(f: A => B): B =
    try { f(resource) } finally { resource.close() }

  def write(path: String, content: String): Unit = withResource(newWriter(path)) { w =>
    w.write(content)
  }
  def readString(path: String) = withResource(newReader(path)) { r =>
    r.lines().collect(Collectors.toList()).asScala.mkString("\n")
  }
}

class LocalFileSystem(override val basePath: String) extends FileSystem {
  private[this] val base = Paths.get(basePath)

  private[this] def prepareWrite(path: String): Unit = {
    val _ = Files.createDirectories(base.resolve(path).getParent)
  }

  override val protocol = "file"

  override def prepare() = {
    val _ = Files.createDirectories(base)
  }
  override def newInputStream(path: String) =
    new BufferedInputStream(Files.newInputStream(base.resolve(path)))
  override def newOutputStream(path: String) = {
    prepareWrite(path)
    new BufferedOutputStream(Files.newOutputStream(base.resolve(path)))
  }

  override def list(path: String) = {
    Files.list(base.resolve(path))
      .collect(Collectors.toList())
      .asScala
      .map(_.getFileName.toString)
  }

  override def exists(path: String) =
    Files.exists(base.resolve(path))
}

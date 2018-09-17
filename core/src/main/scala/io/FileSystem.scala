package com.todesking.scalanb.io

import java.nio.file.Paths
import java.nio.file.Files
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

object FileSystem {
  def newFS(fsName: String, args: Map[String, String]): FileSystem = {
    val loader = java.util.ServiceLoader.load(classOf[FileSystemFactory])
    loader.iterator.asScala
      .toSeq
      .filter(_.name == fsName)
      .headOption
      .getOrElse { throw new RuntimeException(s"Unknown filesystem: $fsName") }
      .newFS(args)
  }
}

trait FileSystemFactory {
  val name: String
  def newFS(args: Map[String, String]): FileSystem
}


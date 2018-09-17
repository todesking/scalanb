package com.todesking.scalanb.io

import java.nio.file.Paths
import java.nio.file.Files
import java.io.{ BufferedInputStream, BufferedOutputStream }
import java.util.stream.Collectors

import scala.collection.JavaConverters._

class LocalFileSystemFactory extends FileSystemFactory {
  override val homePath = sys.props("user.home")
  override val name = "file"
  override def newFS(path: String) = {
    new LocalFileSystem(path)
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

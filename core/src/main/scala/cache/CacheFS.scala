package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.util.Digest

class CacheFS(val underlying: FileSystem, val className: String) {
  def protocol: String = underlying.protocol

  def localPath(id: DepID, parts: String*): String =
    s"${CacheFS.pathString(className, id)}/${parts.mkString("/")}"

  def uri(id: DepID, parts: String*): String =
    s"$protocol://${underlying.basePath}/${localPath(id, parts: _*)}"

  def write(id: DepID, data: Array[Byte]): Unit =
    underlying.writeBytes(localPath(id), data)

  def read(id: DepID): Option[Array[Byte]] = {
    val p = localPath(id)
    if (underlying.exists(p)) Some(underlying.readBytes(p))
    else None
  }
}

object CacheFS {
  def pathString(className: String, id: DepID): String = {
    s"$className/${id.path.mkString("/")}/${Digest.hex(id.stringForDigest)}"
  }
}

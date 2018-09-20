package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.util.Digest

class CacheFS(val underlying: FileSystem, val className: String) {
  def protocol: String = underlying.protocol

  def path(id: DepID): String =
    s"${underlying.basePath}/${CacheFS.pathString(className, id)}"

  def write(id: DepID, data: Array[Byte]): Unit =
    underlying.writeBytes(path(id), data)

  def read(id: DepID): Option[Array[Byte]] = {
    val p = path(id)
    if (underlying.exists(p)) Some(underlying.readBytes(p))
    else None
  }
}

object CacheFS {
  def pathString(className: String, id: DepID): String = {
    s"$className/${id.path.mkString("/")}/${Digest.hex(id.stringForDigest)}"
  }
}

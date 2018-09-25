package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.util.Digest

class CacheFS(val underlying: FileSystem, val className: String) {
  def localPath(id: DepID, parts: String*): String =
    s"${CacheFS.pathString(className, id)}/${parts.mkString("/")}"

  def uri(id: DepID, parts: String*): String =
    s"${underlying.baseUri}/${localPath(id, parts: _*)}"

  def writeBytes(id: DepID, parts: String*)(data: Array[Byte]): Unit =
    underlying.writeBytes(localPath(id, parts: _*), data)

  def readBytes(id: DepID, parts: String*): Option[Array[Byte]] = {
    val p = localPath(id)
    if (underlying.exists(p)) Some(underlying.readBytes(p))
    else None
  }

  def writeString(id: DepID, parts: String*)(data: String): Unit =
    writeBytes(id, parts: _*)(data.getBytes)
  def readString(id: DepID, parts: String*) =
    readBytes(id, parts: _*).map { data => new String(data) }

  def exists(id: DepID): Boolean =
    underlying.exists(localPath(id))
}

object CacheFS {
  def pathString(className: String, id: DepID): String = {
    s"$className/${id.path.mkString("/")}/${Digest.hex(id.stringForDigest)}"
  }
}

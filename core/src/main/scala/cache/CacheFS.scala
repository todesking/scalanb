package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem

class CacheFS(val underlying: FileSystem, nbID: String) {
  def protocol: String = underlying.protocol

  def path(id: DepID): String = CacheFS.pathString(nbID, id)

  def write(id: DepID, data: Array[Byte]): Unit =
    underlying.writeBytes(path(id), data)

  def read(id: DepID): Option[Array[Byte]] = {
    val p = path(id)
    if (underlying.exists(p)) Some(underlying.readBytes(p))
    else None
  }
}

object CacheFS {
  def pathString(nbID: String, id: DepID): String = {
    val sha1 = java.security.MessageDigest.getInstance("SHA-1")
    val digest = sha1.digest(id.stringForDigest.getBytes)
    val digestString = digest.map { x => f"$x%02x" }.mkString("")
    s"$nbID/${id.name}/$digestString"
  }
}

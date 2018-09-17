package com.todesking.scalanb.cache

trait IO {
  def protocol: String
  def path(id: DepID): String
  def write(id: DepID, data: Array[Byte]): Unit
  def read(id: DepID): Option[Array[Byte]]
}

object IO {
  def pathString(id: DepID): String = {
    val sha1 = java.security.MessageDigest.getInstance("SHA-1")
    val digest = sha1.digest(id.stringForDigest.getBytes)
    val digestString = digest.map { x => f"$x%02x" }.mkString("")
    s"${id.name}/$digestString"
  }
  class File(basePath: String) extends IO {
    import java.nio.file.{ Files, Paths }
    override def protocol = "file"
    override def path(id: DepID) = {
      s"$basePath/${pathString(id)}"
    }
    override def write(id: DepID, data: Array[Byte]) = {
      val _ = Files.write(Paths.get(path(id)), data)
    }
    override def read(id: DepID): Option[Array[Byte]] = {
      val p = Paths.get(path(id))
      if (Files.exists(p)) Some(Files.readAllBytes(p))
      else None
    }
  }
}

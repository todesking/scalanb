package com.todesking.scalanb.util

object Digest {
  def hex(s: String, length: Int = 40): String = {
    require(0 < length && length <= 40)
    val sha1 = java.security.MessageDigest.getInstance("SHA-1")
    val digest = sha1.digest(s.getBytes)
    digest.map { x => f"$x%02x" }.mkString("").substring(0, length)
  }
}

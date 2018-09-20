package com.todesking.scalanb.util

object Digest {
  def hex(s: String): String = {
    val sha1 = java.security.MessageDigest.getInstance("SHA-1")
    val digest = sha1.digest(s.getBytes)
    digest.map { x => f"$x%02x" }.mkString("")
  }
}

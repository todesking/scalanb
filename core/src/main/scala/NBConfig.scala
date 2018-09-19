package com.todesking.scalanb

case class NBConfig(
  showTimeMillis: Long,
  errorFormat: ErrorFormat)

object NBConfig {
  val default = NBConfig(showTimeMillis = 5000, errorFormat = ErrorFormat.default)
}


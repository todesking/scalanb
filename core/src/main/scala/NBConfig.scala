package com.todesking.scalanb

case class NBConfig(
  showTimeMillis: Long)

object NBConfig {
  val default = NBConfig(showTimeMillis = 5000)
}


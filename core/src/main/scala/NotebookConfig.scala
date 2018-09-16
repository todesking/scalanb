package com.todesking.scalanb

case class NotebookConfig(
  showTimeMillis: Long)

object NotebookConfig {
  val default = NotebookConfig(showTimeMillis = 5000)
}


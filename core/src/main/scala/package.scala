package com.todesking

package object scalanb {
  def markdown(src: String)(implicit b: Builder): Unit = {
    b.quiet() // Cancel current execution log
    b.markdown(src)
  }
  def setShowTimeMillis(l: Long)(implicit b: Builder): Unit =
    b.setShowTimeMillis(l)
}

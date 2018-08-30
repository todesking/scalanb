package com.todesking.scalanb.format

object Time {
  def fromMillis(millis: Long): String = {
    def hhmmss = f"${millis / 1000 / 60 / 60}%02d:${millis / 1000 / 60 % 60}%02d:${millis / 1000 % 60}%02d"
    val sec = millis / 1000.0
    var text = f"$sec%.2f[Sec]"
    if (sec > 120) text = s"$text($hhmmss)"
    text
  }
}

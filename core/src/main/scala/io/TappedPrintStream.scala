package com.todesking.scalanb.io

// TODO: Support ALL methods
abstract class TappedPrintStream(original: java.io.PrintStream, silent: Boolean) extends java.io.PrintStream(original) {
  override def println(s: String) =
    print(s + "\n")
  override def print(s: String) = {
    tap(s)
    if (!silent)
      original.print(s)
  }
  protected def tap(s: String): Unit
}
object TappedPrintStream {
  def apply(o: java.io.PrintStream, silent: Boolean)(f: String => Unit): TappedPrintStream = new TappedPrintStream(o, silent) {
    override def tap(s: String) = f(s)
  }
}

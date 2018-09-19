package com.todesking.scalanb

sealed abstract class Event
object Event {
  case class Markdown(src: String) extends Event
  case class Expr(value: Value) extends Event
  case class Code(src: String) extends Event
  case class Error(error: Throwable, format: ErrorFormat) extends Event
  case class WholeCode(src: String) extends Event
  case class Quiet() extends Event
  case class StdOut(content: String) extends Event
  case class StdErr(content: String) extends Event
  case class Display(value: Value) extends Event
  case class Finish() extends Event
}

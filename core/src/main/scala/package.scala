package com.todesking

import com.todesking.scalanb.format.Table
import scala.language.experimental.macros

package object scalanb {
  def markdown(src: String)(implicit ctx: NBContext): Unit = {
    ctx.event.quiet() // Cancel current execution log
    ctx.event.markdown(src)
  }

  def setShowTimeMillis(l: Long)(implicit ctx: NBContext): Unit =
    ctx.setShowTimeMillis(l)

  def table(colNames: Seq[String], rows: Seq[Seq[String]]): Value =
    Table.table(colNames, rows)

  def vtable(colNames: Seq[String], rows: Seq[Seq[String]]): Value =
    Table.vtable(colNames, rows)

  def display[A](value: A)(implicit ctx: NBContext, format: Format[A]): Unit =
    ctx.event.display(format(value))

  def inspect[A](body: A): A = macro Inspect.apply[A]

  def checkpoint(implicit ctx: NBContext) = ctx.checkpoint
}

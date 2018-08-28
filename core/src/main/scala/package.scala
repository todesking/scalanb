package com.todesking

import com.todesking.scalanb.format.Table

package object scalanb {
  def markdown(src: String)(implicit b: Builder): Unit = {
    b.quiet() // Cancel current execution log
    b.markdown(src)
  }
  def setShowTimeMillis(l: Long)(implicit b: Builder): Unit =
    b.setShowTimeMillis(l)
  def table(colNames: Seq[String], rows: Seq[Seq[String]]): Value =
    Table.table(colNames, rows)
  def vtable(colNames: Seq[String], rows: Seq[Seq[String]]): Value =
    Table.vtable(colNames, rows)

  def display[A](value: A)(implicit b: Builder, format: Format[A]): Unit =
    b.display(format(value))
}

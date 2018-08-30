package com.todesking.scalanb.format

import com.todesking.scalanb.Value

object Table {
  private[this] def strWidth(s: String) = s.toCharArray.map {
    case c if c < 256 => 1
    case _ => 2
  }.sum

  private[this] def lpad(str: String, w: Int): String = {
    val sw = strWidth(str)
    if (sw < w) " " * (w - sw) + str
    else str
  }

  private[this] def colWidths(rows: Seq[Seq[String]]): Seq[Int] = {
    require(rows.nonEmpty)
    val colSize = rows.head.size
    require(rows.forall(_.size == colSize))
    (0 until colSize).map { i =>
      rows.map { row => strWidth(row(i)) }.max
    }
  }
  private[this] def h(s: String): String = s
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll("\"", "&quot;")
    .replaceAll("'", "&#39;")

  private[this] def wbr(s: String): String = s
    .replaceAll("=", "=<wbr>")
    .replaceAll("&amp;", "&amp;<wbr>")
    .replaceAll("%", "%<wbr>")

  private[this] def requireTable(colNames: Seq[String], rows: Seq[Seq[String]]) =
    require(rows.forall { r => r.size == colNames.size })

  private[this] def textTable(colNames: Seq[String], rows: Seq[Seq[String]]): String = {
    val colSize = colNames.size
    val widths = colWidths(colNames +: rows)
    val header = colNames.zip(widths).map { case (s, w) => lpad(s, w) }.mkString(", ")
    val body = rows.map { row =>
      row.zip(widths).map { case (s, w) => lpad(s, w) }.mkString(", ")
    }
    (header +: body).mkString("\n")
  }

  private[this] def htmlTable(colNames: Seq[String], rows: Seq[Seq[String]]): String = {
    s"""<table style="word-wrap: break-word; font-family: monospace">
      <thead>${
      colNames.map { s => s"<th>${h(s)}</th>" }.mkString("")
    }</thead>
      <tbody>${
      rows.map { row =>
        s"""<tr>\n${row.map { s => s"""<td style="text-align:right">${wbr(h(s))}</td>""" }.mkString("\n")}\n</tr>"""
      }.mkString("\n")
    }</tbody>
      </table>"""
  }

  def table(colNames: Seq[String], rows: Seq[Seq[String]]): Value = {
    requireTable(colNames, rows)
    Value.text(textTable(colNames, rows)) ++ Value.html(htmlTable(colNames, rows))
  }

  private[this] def textVtable(colNames: Seq[String], rows: Seq[Seq[String]]): String = {
    rows.map {
      case row =>
        textTable(Seq("name", "value"), colNames.zip(row).map { case (k, v) => Seq(k, v) })
    }.zipWithIndex.map { case (t, i) => s"Item ${i + 1}:\n$t" }
      .mkString("\n")
  }
  private[this] def htmlVtable(colNames: Seq[String], rows: Seq[Seq[String]]): String = {
    rows.map {
      case row =>
        htmlTable(Seq("name", "value"), colNames.zip(row).map { case (k, v) => Seq(k, v) })
    }.map { t =>
      s"<li>$t</li>"
    }.mkString("<ol>\n", "\n", "\n</ol>")
  }

  def vtable(colNames: Seq[String], rows: Seq[Seq[String]]): Value = {
    require(rows.forall { r => r.size == colNames.size })
    Value.text(textVtable(colNames, rows)) ++ Value.html(htmlVtable(colNames, rows))
  }

}

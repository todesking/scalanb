package com.todesking.scalanb.format

import com.todesking.scalanb.Value

object Table {
  case class Col(content: String, rowspan: Int = 1, header: Boolean = false)
  private[this] def renderHtml(rows: Seq[Seq[Col]]): String = {
    s"""<table style="word-wrap: break-word; font-family: monospace">
    ${
      rows.map { row =>
        s"""<tr>${
          row.map {
            case Col(content, rowspan, header) =>
              val td = if (header) "th" else "td"
              val rs = if (rowspan > 1) s"""rowspan="${h(rowspan)}"""" else ""
              s"""<$td style="text-align: right" ${rs}>${wbr(h(content))}</$td>"""
          }.mkString("")
        }</tr>"""
      }.mkString("\n")
    }
</table>"""
  }
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
  private[this] def h(s: Any): String = s.toString
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll("\"", "&quot;")
    .replaceAll("'", "&#39;")

  private[this] def wbr(s: String): String = s
    .replaceAll("=", "=<wbr></wbr>")
    .replaceAll("&amp;", "&amp;<wbr></wbr>")
    .replaceAll("%", "%<wbr></wbr>")

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

  private[this] def htmlTable(colNames: Seq[String], rows: Seq[Seq[String]]): String =
    renderHtml(colNames.map { c => Col(c, header = true) } +: rows.map(_.map { c => Col(c) }))

  def table(rows: Seq[Seq[Col]]) = renderHtml(rows)
  def table(colNames: Seq[String], rows: Seq[Seq[String]]): Value = {
    requireTable(colNames, rows)
    Value.text(textTable(colNames, rows)) ++ Value.html(htmlTable(colNames, rows))
  }

  private[this] def textVtable(colNames: Seq[String], rows: Seq[Seq[String]]): String = {
    textTable(
      Seq("item", "name", "value"),
      rows.zipWithIndex.flatMap {
        case (row, i) =>
          colNames.zip(row).map { case (k, v) => Seq((i + 1).toString, k, v) }
      })
  }
  private[this] def htmlVtable(colNames: Seq[String], rows: Seq[Seq[String]]): String = {
    renderHtml(
      rows.zipWithIndex.flatMap {
        case (row, i) =>
          val c = Col(s"${i + 1}", rowspan = colNames.size, header = true)
          row.zip(colNames).zipWithIndex.map {
            case ((v, k), i) =>
              if (i == 0) Seq(c, Col(k, header = true), Col(v))
              else Seq(Col(k, header = true), Col(v))
          }
      })
  }

  def vtable(colNames: Seq[String], rows: Seq[Seq[String]]): Value = {
    require(rows.forall { r => r.size == colNames.size })
    Value.text(textVtable(colNames, rows)) ++ Value.html(htmlVtable(colNames, rows))
  }

}

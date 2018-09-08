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

  private[this] def requireTable(colNames: Seq[_], rows: Seq[Seq[_]]) =
    require(rows.forall { r => r.size == colNames.size })

  private[this] def renderText(rows: Seq[Seq[Col]]): String = {
    // TODO: support rowspan
    if (rows.isEmpty) return ""
    val colSize = rows.head.size
    val widths = colWidths(rows.map(_.map(_.content)))
    rows.map { row =>
      row.map(_.content).zip(widths).map { case (s, w) => lpad(s, w) }.mkString(", ")
    }.mkString("\n")
  }
  private[this] def renderCsv(rows: Seq[Seq[Col]]): String = {
    def escape(s: String) = s
      .replaceAll("\"", "\"\"")
    def colString(s: String) = if (s.contains("\"")) s""""${escape(s)}"""" else s
    rows.map(_.map(_.content)).map(_.map(colString).mkString(",")).mkString("\n")
  }

  def table(colNames: Seq[String], rows: Seq[Seq[String]]): Value = {
    requireTable(colNames, rows)
    table(colNames.map(Col.apply(_, header = true)) +: rows.map(_.map(Col.apply(_))))
  }

  def table(rows: Seq[Seq[Col]]): Value = {
    require(rows.map(_.size).distinct.size == 1)
    Value.html(renderHtml(rows)) ++ Value.text(renderText(rows)) ++ Value.csv(renderCsv(rows))
  }

  private[this] def textVtable(colNames: Seq[String], rows: Seq[Seq[String]]): String = {
    renderText(
      Seq("item", "name", "value").map(Col(_, header = true)) +: rows.zipWithIndex.flatMap {
        case (row, i) =>
          colNames.zip(row).map { case (k, v) => Seq((i + 1).toString, k, v).map(Col(_)) }
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

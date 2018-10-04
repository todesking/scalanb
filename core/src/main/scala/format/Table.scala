package com.todesking.scalanb.format

import com.todesking.scalanb.Value
import com.todesking.scalanb.format.Html.h

object Table {
  sealed abstract class Col {
    def text: String
  }
  object Col {
    def apply(text: String, header: Boolean = false) = Filled(text, header)
    case class Filled(override val text: String, header: Boolean) extends Col
    case object Empty extends Col {
      override val text = ""
    }
  }
  private[this] def renderHtml(rows: Seq[Seq[Col]]): String = {
    // spans((i, j)) = (rowspan_ij, colspan_ij)
    val spans: Map[(Int, Int), (Int, Int)] =
      rows.zipWithIndex.flatMap {
        case (row, ri) =>
          row.zipWithIndex.flatMap {
            case (Col.Filled(_, _), ci) =>
              var rj = ri + 1
              while (rj < rows.size && rows(rj)(ci) == Col.Empty) {
                rj += 1
              }
              val rowspan = rj - ri

              var cj = ci + 1
              while (cj < row.size && rows(ri)(cj) == Col.Empty) {
                cj += 1
              }
              val colspan = cj - ci
              Some((ri, ci) -> ((rowspan, colspan)))
            case (Col.Empty, _) => None
          }
      }.toMap
    s"""<table style="word-wrap: break-word; font-family: monospace">
    ${
      rows.zipWithIndex.map {
        case (row, ri) =>
          s"""<tr>${
            row.zipWithIndex.map {
              case (Col.Empty, _) =>
                ""
              case (Col.Filled(content, header), ci) =>
                val (rowspan, colspan) = spans((ri, ci))
                val td = if (header) "th" else "td"
                val rs = if (rowspan > 1) s"""rowspan="${h(rowspan.toString)}"""" else ""
                val cs = if (colspan > 1) s"""colspan="${h(colspan.toString)}"""" else ""
                s"""<$td style="text-align: right" $rs $cs>${wbr(h(content))}</$td>"""
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

  private[this] def wbr(s: String): String = s
    .replaceAll("=", "=<wbr></wbr>")
    .replaceAll("&amp;", "&amp;<wbr></wbr>")
    .replaceAll("%", "%<wbr></wbr>")

  private[this] def renderText(rows: Seq[Seq[Col]]): String = {
    if (rows.isEmpty) return ""
    val colSize = rows.head.size
    val widths = colWidths(rows.map(_.map(_.text)))
    rows.map { row =>
      row.map(_.text).zip(widths).map { case (s, w) => lpad(s, w) }.mkString(", ")
    }.mkString("\n")
  }
  private[this] def renderCsv(rows: Seq[Seq[Col]]): String = {
    def escape(s: String) = s
      .replaceAll("\"", "\"\"")
    def colString(s: String) = if (s.contains("\"")) s""""${escape(s)}"""" else s
    rows.map(_.map(_.text)).map(_.map(colString).mkString(",")).mkString("\n")
  }

  def table(colNames: Seq[String], rows: Seq[Seq[String]]): Value = {
    require(rows.forall { r => r.size == colNames.size })
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
          row.zip(colNames).zipWithIndex.map {
            case ((v, k), j) =>
              val header = if (j == 0) Col(s"${i + 1}", header = true) else Col.Empty
              Seq(header, Col(k, header = true), Col(v))
          }
      })
  }

  def vtable(colNames: Seq[String], rows: Seq[Seq[String]]): Value = {
    require(rows.forall { r => r.size == colNames.size })
    Value.text(textVtable(colNames, rows)) ++ Value.html(htmlVtable(colNames, rows))
  }

}

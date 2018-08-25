package com.todesking.scalanb.spark

import org.apache.spark.sql.Dataset

import com.todesking.scalanb.Value
import com.todesking.scalanb.Format
import com.todesking.scalanb.format.Table

object Implicits {
  implicit class DatasetNB[A](val self: Dataset[A]) {
    def nb: ImplicitsUtil.NBOps[A] = new ImplicitsUtil.NBOps[A](self)
  }

  implicit def datasetFormat[A]: Format[Dataset[A]] = Format[Dataset[A]] { ds =>
    ImplicitsUtil.formatDataset(ds)
  }
}

object ImplicitsUtil {
  class NBOps[A](self: Dataset[A]) {
    private[this] def build(n: Int): (Seq[String], Seq[Seq[String]]) = {
      val df = self.toDF()
      val cols = df.columns.toSeq
      val rows = df.limit(n).collect().map { row =>
        (0 until cols.size).map { i =>
          val value = row.get(i)
          // TODO: format value
          s"$value"
        }
      }.toSeq
      (cols, rows)
    }
    def show(n: Int = 10): Value = {
      val (cols, rows) = build(n)
      Table.table(cols, rows)
    }
    def vshow(n: Int = 10): Value = {
      val (cols, rows) = build(n)
      Table.vtable(cols, rows)
    }
  }

  def formatDataset(ds: Dataset[_]): Value = {
    Value.text(ds.schema.treeString)
  }
}

package com.todesking.scalanb.spark

import org.apache.spark.sql.Dataset
import org.apache.spark.sql.types

import com.todesking.scalanb.Value
import com.todesking.scalanb.Format
import com.todesking.scalanb.format.Table

trait Implicits {
  implicit class DatasetNB[A](val self: Dataset[A]) {
    def nb: ImplicitsUtil.NBOps[A] = new ImplicitsUtil.NBOps[A](self)
  }

  implicit def datasetFormat[A]: Format[Dataset[A]] = Format[Dataset[A]] { ds =>
    ImplicitsUtil.formatDataset(ds)
  }
}

object AllImplicits extends Implicits with cache.Implicits

object ImplicitsUtil {
  class NBOps[A](self: Dataset[A]) {
    private[this] def build(n: Int): (Seq[String], Seq[Seq[String]]) = {
      val df = self.toDF()
      val cols = df.columns.toSeq
      val rows = df.limit(n).collect().map { row =>
        (0 until cols.size).map { i =>
          val value = row.get(i)
          df.schema(i).dataType match {
            case types.DoubleType if value != null =>
              val d = value.asInstanceOf[Double]
              if (d == 0.0 || d > 0.001) f"$d%.2f"
              else s"$d"
            case _ =>
              s"$value"
          }
        }
      }.toSeq
      (cols, rows)
    }
    def show(n: Int = 10): Value = table(n)
    def table(n: Int = 10): Value = {
      val (cols, rows) = build(n)
      Table.table(cols, rows)
    }
    def vshow(n: Int = 10): Value =vtable(n)
    def vtable(n: Int = 10): Value = {
      val (cols, rows) = build(n)
      Table.vtable(cols, rows)
    }
  }

  def formatDataset(ds: Dataset[_]): Value = {
    Value.text(ds.schema.treeString)
  }
}

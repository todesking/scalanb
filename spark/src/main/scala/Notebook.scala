package com.todesking.scalanb.spark

import com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.language.experimental.macros

class Notebook extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Notebook.macroExpand
}
object Notebook {
  class MacroImpl[A <: Context](c: A) extends scalanb.Notebook.MacroImpl[A](c) {
    import context.Expr
    import context.TypeName
    import context.universe.Tree
    import context.universe.Quasiquote

    override def args =
      super.args :+ q"spark: _root_.org.apache.spark.sql.SparkSession"

    override def prelude =
      super.prelude ++ Seq(
        q"import _root_.com.todesking.scalanb.spark.Implicits._",
        q"import spark.implicits._")

    override def makeMain(tpname: TypeName, notebookName: String): Tree = {
      q"""
      def main(args: Array[String]): Unit = {
        _root_.com.todesking.scalanb.spark.Runner.runBatch(args, $notebookName) { (builder, spark) =>
          new $tpname()(builder, spark)
        }
      }
      """
    }
  }
  def macroExpand(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    new MacroImpl[c.type](c).apply(annottees: _*)
}


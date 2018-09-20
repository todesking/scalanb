package com.todesking.scalanb.spark

import com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

import scala.language.experimental.macros

class Notebook extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Notebook.macroExpand
}
object Notebook {
  class MacroImpl[A <: Context](c: A) extends scalanb.Notebook.MacroImpl[A](c) {
    import context.TypeName
    import context.universe.Tree
    import context.universe.Quasiquote

    override def args =
      super.args :+ q"spark: _root_.org.apache.spark.sql.SparkSession"

    override def prelude =
      super.prelude ++ Seq(
        q"import spark.implicits._",
        q"import _root_.com.todesking.scalanb.spark.AllImplicits._")

    override def makeMain(tpname: TypeName, nbName: String, nbClassName: Tree): Tree = {
      q"""
      def main(args: Array[String]): Unit = {
        _root_.com.todesking.scalanb.spark.Runner.runBatch(args, $nbName, $nbClassName, this.scalanb__source) { (ctx, spark) =>
          new $tpname()(ctx, spark)
        }
      }
      """
    }
  }
  def macroExpand(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    new MacroImpl[c.type](c).apply(annottees: _*)
}


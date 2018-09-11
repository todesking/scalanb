package com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.language.experimental.macros

class Notebook extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Notebook.macroExpand
}

object Notebook {
  class MacroImpl[A <: Context](val context: A) {
    import context.Expr
    import context.TypeName
    import context.universe.Tree
    import context.universe.Quasiquote
    import context.universe.ClassDef
    import context.universe.ModuleDef

    def makeMain(tpname: TypeName, notebookName: String): Tree = {
      q"""
      def main(args: Array[String]): Unit = {
        _root_.com.todesking.scalanb.Runner.runBatch(args, $notebookName) { builder => new $tpname()(builder) }
      }
      """
    }

    def args: Seq[Tree] = {
      Seq(q"scalanb__builder: _root_.com.todesking.scalanb.Builder")
    }

    def prelude: Seq[Tree] = Seq()

    def apply(annottees: Expr[Any]*): Expr[Any] = {
      annottees.map(_.tree) match {
        case Seq(q"class $tpname { ..$stats }") =>
          transform(tpname, stats, Seq())
        case Seq(q"class $tpname { ..$stats }", q"object $oname { ..$ostats }") =>
          transform(tpname, stats, ostats)
      }
    }

    def transform(tpname: TypeName, stats: Seq[Tree], ostats: Seq[Tree]): Expr[Any] = {
      val notebookName = tpname.toString
      val mainMethod = makeMain(tpname, notebookName)
      Expr[Any](q"""
            class $tpname(implicit ..${args}) {
              ..$prelude
              ..${Inspect.transform(context)(stats)}
            }
            object ${tpname.toTermName} {
              $mainMethod
              ..$ostats
            }
          """)
    }
  }

  def macroExpand(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    new MacroImpl[c.type](c).apply(annottees: _*)
}

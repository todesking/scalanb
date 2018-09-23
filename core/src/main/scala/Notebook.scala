package com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

import com.todesking.scalanb.util.MacroUtil

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

    val util = MacroUtil.bind[context.type](context)

    def makeMain(tpname: TypeName, nbName: String, nbClassName: Tree): Tree = {
      q"""
      def main(args: Array[String]): Unit = {
        _root_.com.todesking.scalanb.Runner.runBatch(args, $nbName, $nbClassName, this.scalanb__source) { ctx => new $tpname()(ctx) }
      }
      """
    }

    def args: Seq[Tree] = {
      Seq(q"scalanb__context: _root_.com.todesking.scalanb.NBContext")
    }

    def prelude: Seq[Tree] = Seq()

    def apply(annottees: Expr[Any]*): Expr[Any] = {
      annottees.map(_.tree) match {
        case Seq(cdef @ q"class $tpname { ..$stats }") =>
          val src = util.wholeSource(stats)
          transform(tpname, stats, Seq(), src)
        case Seq(cdef @ q"class $tpname { ..$stats }", q"object $oname { ..$ostats }") =>
          val src = util.wholeSource(stats)
          transform(tpname, stats, ostats, src)
      }
    }

    def transform(tpname: TypeName, stats: Seq[Tree], ostats: Seq[Tree], src: String): Expr[Any] = {
      val nbName = tpname.toString
      val nbClassName = q"_root_.scala.Predef.classOf[$tpname].getName"
      val mainMethod = makeMain(tpname, nbName, nbClassName)
      Expr[Any](q"""
            class $tpname(implicit ..$args) {
              ..$prelude
              ..${Inspect.transform(context)(stats, true)}
            }
            object ${tpname.toTermName} {
              $mainMethod
              def scalanb__source: _root_.java.lang.String = ${util.stringLiteral(src)}
              ..$ostats
            }
          """)
    }
  }

  def macroExpand(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    new MacroImpl[c.type](c).apply(annottees: _*)
}

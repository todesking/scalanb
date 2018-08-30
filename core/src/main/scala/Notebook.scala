package com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.language.experimental.macros

class Notebook extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Notebook.macroImpl.apply
}

object Notebook {
  trait MacroImpl {
    def makeRunMethod(c: Context)(stats: Seq[c.universe.Tree]): c.universe.Tree = {
      import c.universe._
      q"""
          def scalanb__run(implicit scalanb__builder: _root_.com.todesking.scalanb.Builder): _root_.scala.Unit = {
              ${Inspect.transform(c)(stats)}
          }"""
    }

    def makeMain(c: Context)(tpname: c.universe.TypeName, notebookName: String): c.universe.Tree = {
      import c.universe._
      q"""
      def main(args: Array[String]): Unit = {
        val target = new ${tpname}()
        _root_.com.todesking.scalanb.Runner.runBatch(args, $notebookName, target)
      }
      """
    }

    def apply(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      annottees.map(_.tree) match {
        case Seq(q"class $tpname () { ..$stats }") =>
          val notebookName = tpname.toString
          val runMethod = makeRunMethod(c)(stats)
          val mainMethod = makeMain(c)(tpname, notebookName)
          c.Expr[Any](q"""
            class $tpname() {
              $runMethod
            }
            object ${tpname.toTermName} {
              $mainMethod
            }
          """)
      }
    }
  }
  object macroImpl extends MacroImpl
}

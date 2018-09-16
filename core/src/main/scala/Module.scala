package com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.language.experimental.macros

class Module extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Module.macroImpl.apply
}

object Module {
  trait MacroImpl {
    def apply(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      annottees.map(_.tree) match {
        case Seq(q"class $tpname (...$args) { ..$stats }") =>
          val ctx = q"_root_.scala.Predef.implicitly[_root_.com.todesking.scalanb.NotebookContext]"
          val moduleName = tpname.toString
          c.Expr[Any](q"""
            class $tpname(...$args)(implicit scalanb__context: _root_.com.todesking.scalanb.NotebookContext)  {
              $ctx.event.markdown("```\nEntering module: " + $moduleName + " \n```")
              ..${Inspect.transform(c)(stats)}
              $ctx.event.markdown("```\nDone: " + $moduleName + " \n```")
            }
          """)
      }
    }
  }
  object macroImpl extends MacroImpl
}


package com.todesking.scalanb

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.language.experimental.macros

class Module extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Module.MacroImpl.apply
}

object Module {
  class MacroImpl(val c: Context) {
    import c.Expr
    import c.TypeName
    import c.universe.Tree
    import c.universe.Quasiquote

    def apply(annottees: Expr[Any]*): Expr[Any] = {
      annottees.map(_.tree) match {
        case Seq(q"class $tpname (...$args) { ..$stats }") =>
          transform(tpname, args, stats, Seq())
        case Seq(q"class $tpname (...$args) { ..$stats }", q"object $oname { ..$ostats }") =>
          transform(tpname, args, stats, ostats)
      }
    }

    def transform(tpname: TypeName, args: Seq[Seq[Tree]], stats: Seq[Tree], ostats: Seq[Tree]): Expr[Any] = {
      val className = q"_root_.scala.Predef.classOf[$tpname].getName()"
      val ctx = q"_root_.scala.Predef.implicitly[_root_.com.todesking.scalanb.NBContext]"
      val mctx = q"_root_.scala.Predef.implicitly[_root_.com.todesking.scalanb.ModuleContext]"
      val moduleName = tpname.toString
      val argNames = args.map(_.map {
        case q"$mods val $name: $tpe = $expr" => name
      })
      val loadArgs = args.map(_.map {
        case q"$mods val $name: $tpe = $expr" => q"val $name: $tpe = $expr"
      })
      Expr[Any](q"""
        class $tpname private (...$args)(implicit scalanb__context: _root_.com.todesking.scalanb.NBContext)  {
          $ctx.event.markdown("```\nEntering module: " + $moduleName + " \n```")
          ..${Inspect.transform(c)(stats)}
          $ctx.event.markdown("```\nDone: " + $moduleName + " \n```")
        }
        object ${tpname.toTermName} {
          def load(...$loadArgs)(implicit ctx: _root_.com.todesking.scalanb.NBContext): $tpname = {
            ctx.loadModule[$tpname]($moduleName, $className) {  c => new $tpname(...$argNames)(c) }
          }
          ..$ostats
        }
      """)
    }
  }
}


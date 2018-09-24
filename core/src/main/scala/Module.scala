package com.todesking.scalanb

import com.todesking.scalanb.util.MacroUtil

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

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

    val util = MacroUtil.bind[c.type](c)

    def apply(annottees: Expr[Any]*): Expr[Any] = {
      annottees.map(_.tree) match {
        case Seq(q"class $tpname (...$args) { ..$stats }") =>
          transform(tpname, args, stats, Seq())
        case Seq(q"class $tpname (...$args) { ..$stats }", q"object $oname { ..$ostats }") =>
          transform(tpname, args, stats, ostats)
      }
    }

    def transform(tpname: TypeName, args: Seq[Seq[Tree]], stats: Seq[Tree], ostats: Seq[Tree]): Expr[Any] = {
      stats.foreach(util.register)
      ostats.foreach(util.register)

      val className = q"_root_.scala.Predef.classOf[$tpname].getName()"
      val moduleName = tpname.toString
      val argNames = args.map(_.map {
        case q"$mods val $name: $tpe = $expr" => name
      })
      val loadArgs = args.map(_.map {
        case q"$mods val $name: $tpe = $expr" => q"val $name: $tpe = $expr"
      })
      val classTree = q"""
        class $tpname private (...$args)(implicit scalanb__context: _root_.com.todesking.scalanb.NBContext)  {
          ..${Inspect.transform(c)(stats, true)}
        }
      """

      import c.universe._
      val objTree = q"""
      object ${tpname.toTermName} {
        def load(...$loadArgs)(implicit ctx: _root_.com.todesking.scalanb.NBContext): $tpname = {
          ctx.loadModule[$tpname]($moduleName, $className) {  c => new $tpname(...$argNames)(c) }
        }
        val scalanb__source = ${util.stringLiteral(util.wholeSource(stats))}
        ..$ostats
      }"""

      c.Expr[Any](q"""$classTree; $objTree""")
    }
  }
}


package com.todesking.scalanb

import com.todesking.scalanb.util.MacroUtil

import scala.reflect.macros.blackbox.Context

object Inspect {
  def apply[A: c.WeakTypeTag](c: Context)(body: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val impl = new MacroImpl[c.type](c)
    body.tree match {
      case Block(stats, expr) => impl.transform(stats :+ expr, false)
      case st => impl.transform(Seq(st), false)
    }
  }

  def transform[A: c.WeakTypeTag](c: Context)(trees: Seq[c.Tree], discardAllValues: Boolean): c.Expr[A] =
    new MacroImpl[c.type](c).transform(trees, discardAllValues)

  class MacroImpl[C <: Context](val c: C) {
    import c.Expr
    import c.WeakTypeTag
    import c.Tree
    import c.universe.Quasiquote

    val util = MacroUtil.bind[c.type](c)

    def transform[A: WeakTypeTag](trees: Seq[Tree], discardAllValues: Boolean): Expr[A] = {
      val newBody = processStats(trees, discardAllValues)
      Expr[A](q"{ ..$newBody }")
    }

    private[this] def processStats(stats: Seq[Tree], discardAllValues: Boolean): Seq[Tree] = {
      import c.universe._
      val ctx = q"_root_.scala.Predef.implicitly[_root_.com.todesking.scalanb.NBContext]"
      val srcs = util.sources(stats)
      srcs.zipWithIndex.flatMap {
        case ((src, stat), i) =>
          val srcStat = src.map { s => q"$ctx.event.code(${util.stringLiteral(s)})" }
          val modStat = stat match {
            case st if st.isDef || st.isType || !st.isTerm => // TODO: I don't know how to detect not-a-value trees
              st
            case expr if !discardAllValues && i == srcs.size - 1 =>
              q"$ctx.event.expr($expr)"
            case expr =>
              q"$ctx.event.exprDiscard($expr)"
          }
          srcStat.toSeq :+ modStat
      }
    }
  }
}

package com.todesking.scalanb

import scala.reflect.macros.blackbox.Context

object Inspect {
  def apply[A: c.WeakTypeTag](c: Context)(body: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val newBody =
      body.tree match {
        case q"{ ..$stats }" => processStats(c)(stats)
        case st => processStat(c)(st)
      }
    c.Expr[A](q"{ ..$newBody }")
  }

  private[this] def processStats(c: Context)(stats: Seq[c.universe.Tree]): Seq[c.universe.Tree] =
    stats.flatMap(processStat(c)(_))

  private[this] def processStat(c: Context)(stat: c.universe.Tree): Seq[c.universe.Tree] = {
    import c.universe._
    val builder = q"_root_.scala.Predef.implicitly[_root_.com.todesking.scalanb.Builder]"
    stat match {
      case st if st.isDef || st.isType || !st.isTerm => // TODO: I don't know how to detect not-a-value trees
        val src = readContent(c)(st)
        Seq(
          q"$builder.code(${Literal(Constant(src))})",
          st)
      case expr =>
        Seq(
          q"$builder.code(${Literal(Constant(readContent(c)(expr)))})",
          q"$builder.expr($expr)")
    }
  }

  private[this] def readContent(c: Context)(t: c.Tree): String = {
    if (t.pos == c.universe.NoPosition || t.pos.source.content.isEmpty) {
      "<source unavailable>"
    } else {
      t.pos.source.content.slice(t.pos.start, t.pos.end + 1).mkString("")
    }
  }
}

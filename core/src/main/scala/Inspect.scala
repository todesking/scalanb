package com.todesking.scalanb

import scala.reflect.macros.blackbox.Context

object Inspect {
  def apply[A: c.WeakTypeTag](c: Context)(body: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    println("BODY: " +
      readContent(c)(body.tree))
    body.tree match {
      case Block(stats, expr) => transform(c)(stats :+ expr)
      case st => transform(c)(Seq(st))
    }
  }

  def transform[A: c.WeakTypeTag](c: Context)(trees: Seq[c.universe.Tree]): c.Expr[A] = {
    import c.universe._
    val newBody = processStats(c)(trees)
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
      def gather(t: c.Tree): (Int, Int) =
        if (t.pos == c.universe.NoPosition) (Int.MaxValue, 0)
        else t.children.foldLeft((t.pos.start, t.pos.end)) {
          case ((start, end), t) =>
            val (start2, end2) = gather(t)
            (math.min(start, start2), math.max(end, end2))
        }
      val (start, end) = gather(t)
      t.pos.source.content.slice(start, end + 1).mkString("")
    }
  }
}

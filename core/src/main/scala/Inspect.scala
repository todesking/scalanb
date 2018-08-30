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
    // Dirty hack to support `val (a, b) = expr` style statements
    // That statements are desugared to `val x$1 = expr match ...; val a = x._1; val b = x._2`
    stats.foldLeft((0, Seq.empty[c.Tree])) {
      case ((pos, trees), t) =>
        val (start, end) = range(c)(t)
        val last = readContent(c)(t).last
        val cont = end <= pos || (end == pos + 1 && (last == ';' || last == '\n'))
        (math.max(end, pos), trees ++ processStat(c)(t, cont))
    }._2

  private[this] def processStat(c: Context)(stat: c.universe.Tree, cont: Boolean): Seq[c.universe.Tree] = {
    import c.universe._
    val builder = q"_root_.scala.Predef.implicitly[_root_.com.todesking.scalanb.Builder]"
    stat match {
      case st if st.isDef || st.isType || !st.isTerm => // TODO: I don't know how to detect not-a-value trees
        if (cont) Seq(st)
        else Seq(q"$builder.code(${content(c)(st)})", st)
      case expr =>
        val tt = q"$builder.expr($expr)"
        if (cont) Seq(tt)
        else Seq(q"$builder.code(${content(c)(expr)})", tt)
    }
  }

  private[this] def content(c: Context)(t: c.Tree): c.Tree = {
    import c.universe._
    Literal(Constant(readContent(c)(t)))
  }

  private[this] def range(c: Context)(t: c.Tree): (Int, Int) =
    if (t.pos == c.universe.NoPosition) (Int.MaxValue, 0)
    else t.children.foldLeft((t.pos.start, t.pos.end)) {
      case ((start, end), t) =>
        val (start2, end2) = range(c)(t)
        (math.min(start, start2), math.max(end, end2))
    }
  private[this] def readContent(c: Context)(t: c.Tree): String = {
    if (t.pos == c.universe.NoPosition || t.pos.source.content.isEmpty) {
      "<source unavailable>"
    } else {
      val (start, end) = range(c)(t)
      t.pos.source.content.slice(start, end + 1).mkString("")
    }
  }
}

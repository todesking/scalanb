package com.todesking.scalanb

import scala.reflect.macros.blackbox.Context

object Inspect {
  def apply[A: c.WeakTypeTag](c: Context)(body: c.Expr[A]): c.Expr[A] = {
    import c.universe._
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

  def wholeSource(c: Context)(trees: Seq[c.universe.Tree]): String =
    sources(c)(trees).flatMap(_._1).mkString("\n")

  private[this] def sources(c: Context)(ts: Seq[c.Tree]): Seq[(Option[String], c.Tree)] =
    // Dirty hack to support `val (a, b) = expr` style statements
    // That statements are desugared to `val x$1 = expr match ...; val a = x._1; val b = x._2`
    ts.foldLeft((0, Seq.empty[(Option[String], c.Tree)])) {
      case ((pos, trees), t) =>
        val (start, end) = range(c)(t)
        val last = source(c)(t).last
        val cont = end <= pos || (end == pos + 1 && (last == ';' || last == '\n'))
        val src = if (cont) None else Some(source(c)(t))
        (math.max(end, pos), trees :+ ((src, t)))
    }._2

  private[this] def processStats(c: Context)(stats: Seq[c.universe.Tree]): Seq[c.universe.Tree] = {
    import c.universe._
    val ctx = q"_root_.scala.Predef.implicitly[_root_.com.todesking.scalanb.NBContext]"
    sources(c)(stats).flatMap {
      case (src, stat) =>
        val srcStat = src.map { s => q"$ctx.event.code($s)" }
        val modStat = stat match {
          case st if st.isDef || st.isType || !st.isTerm => // TODO: I don't know how to detect not-a-value trees
            st
          case expr =>
            q"$ctx.event.expr($expr)"
        }
        srcStat.toSeq :+ modStat
    }
  }

  private[this] def sourceLit(c: Context)(t: c.Tree): c.Tree = {
    import c.universe._
    q"_root_.scala.StringContext.apply(${Literal(Constant(source(c)(t)))}).s()"
  }

  private[this] def rangeUnion(l: (Int, Int), r: (Int, Int)) = (math.min(l._1, r._1), math.max(l._2, r._2))

  private[this] def range(c: Context)(t: c.Tree): (Int, Int) = {
    val noP = (Int.MaxValue, 0)
    val initialP = if (t.pos == c.universe.NoPosition) noP else (t.pos.start, t.pos.end)
    val childrenP =
      t.children.foldLeft(initialP) { (p, t) =>
        rangeUnion(p, range(c)(t))
      }
    import c.universe._
    val specialP = t match {
      case ClassDef(mods, name, tparams, impl) =>
        (tparams.map(range(c)(_)) :+ range(c)(impl)).reduceOption(rangeUnion) getOrElse noP
      case Template(parents, self, body) =>
        ((parents :+ self) ++ body).map(range(c)(_)).reduceOption(rangeUnion) getOrElse noP
      case _ => noP
    }
    rangeUnion(childrenP, specialP)
  }

  // Read raw source code from tree(NOTE: *best effort*)
  def source(c: Context)(t: c.Tree): String = {
    def clean(s: String) = s.replaceAll("""^\s+|\s+$""", "")
    def content(t: c.Tree): Array[Char] = {
      val c = t.pos.source.content
      if (c.nonEmpty) c
      else t.children.map(content).find(_.nonEmpty) getOrElse Array()
    }
    val (start, end) = range(c)(t)
    if (start == Int.MaxValue) "<source unavailable>"
    else clean(content(t).slice(start, end + 1).mkString(""))
  }
}

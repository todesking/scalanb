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

  private[this] def processStats(c: Context)(stats: Seq[c.universe.Tree]): Seq[c.universe.Tree] =
    // Dirty hack to support `val (a, b) = expr` style statements
    // That statements are desugared to `val x$1 = expr match ...; val a = x._1; val b = x._2`
    stats.foldLeft((0, Seq.empty[c.Tree])) {
      case ((pos, trees), t) =>
        val (start, end) = range(c)(t)
        val last = source(c)(t).last
        val cont = end <= pos || (end == pos + 1 && (last == ';' || last == '\n'))
        (math.max(end, pos), trees ++ processStat(c)(t, cont))
    }._2

  private[this] def processStat(c: Context)(stat: c.universe.Tree, cont: Boolean): Seq[c.universe.Tree] = {
    import c.universe._
    val builder = q"_root_.scala.Predef.implicitly[_root_.com.todesking.scalanb.Builder]"
    stat match {
      case st if st.isDef || st.isType || !st.isTerm => // TODO: I don't know how to detect not-a-value trees
        if (cont) Seq(st)
        else Seq(q"$builder.code(${sourceLit(c)(st)})", st)
      case expr =>
        val tt = q"$builder.expr($expr)"
        if (cont) Seq(tt)
        else Seq(q"$builder.code(${sourceLit(c)(expr)})", tt)
    }
  }

  private[this] def sourceLit(c: Context)(t: c.Tree): c.Tree = {
    import c.universe._
    Literal(Constant(source(c)(t)))
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
    def content(t: c.Tree): Array[Char] = {
      val c = t.pos.source.content
      if (c.nonEmpty) c
      else t.children.map(content).find(_.nonEmpty) getOrElse Array()
    }
    val (start, end) = range(c)(t)
    if (start == Int.MaxValue) "<source unavailable>"
    else content(t).slice(start, end + 1).mkString("")
  }
}

package com.todesking.scalanb.util

import scala.reflect.macros.blackbox.Context

object MacroUtil {
  def bind[C <: Context](c: C): Impl[C] = new Impl[C](c)

  class Impl[C <: Context](val c: C) {
    import c.universe.Expr
    import c.universe.Tree

    def stringLiteral(s: String): Expr[String] = {
      import c.universe._
      val tree =
        if (!s.contains("$")) Literal(Constant(s))
        else q"""${Literal(Constant(s.replaceAll("\\$", "\\$-")))}.replaceAll("\\$$-", "\\$$")"""
      c.Expr[String](tree)
    }

    def wholeSource(trees: Seq[Tree]): String =
      sources(trees).flatMap(_._1).mkString("\n")

    def sources(ts: Seq[Tree]): Seq[(Option[String], Tree)] =
      // Dirty hack to support `val (a, b) = expr` style statements
      // That statements are desugared to `val x$1 = expr match ...; val a = x._1; val b = x._2`
      ts.foldLeft((0, Seq.empty[(Option[String], Tree)])) {
        case ((pos, trees), t) =>
          val (start, end) = range(t)
          val last = source(t).last
          val cont = end <= pos || (end == pos + 1 && (last == ';' || last == '\n'))
          val src = if (cont) None else Some(source(t))
          (math.max(end, pos), trees :+ ((src, t)))
      }._2
    private[this] def rangeUnion(l: (Int, Int), r: (Int, Int)) = (math.min(l._1, r._1), math.max(l._2, r._2))

    private[this] def range(t: Tree): (Int, Int) = {
      val noP = (Int.MaxValue, 0)
      val initialP = if (t.pos == c.universe.NoPosition) noP else (t.pos.start, t.pos.end)
      val childrenP =
        t.children.foldLeft(initialP) { (p, t) =>
          rangeUnion(p, range(t))
        }
      import c.universe._
      val specialP = t match {
        case ClassDef(mods, name, tparams, impl) =>
          (tparams.map(range(_)) :+ range(impl)).reduceOption(rangeUnion) getOrElse noP
        case Template(parents, self, body) =>
          ((parents :+ self) ++ body).map(range(_)).reduceOption(rangeUnion) getOrElse noP
        case _ => noP
      }
      rangeUnion(childrenP, specialP)
    }

    // Read raw source code from tree(NOTE: *best effort*)
    def source(t: Tree): String = {
      def clean(s: String) = s.replaceAll("""^\s+|\s+$""", "")
      def content(t: Tree): Array[Char] = {
        val c = t.pos.source.content
        if (c.nonEmpty) c
        else t.children.map(content).find(_.nonEmpty) getOrElse Array()
      }
      val (start, end) = range(t)
      if (start == Int.MaxValue) "<source unavailable>"
      else clean(content(t).slice(start, end + 1).mkString(""))
    }
  }
}

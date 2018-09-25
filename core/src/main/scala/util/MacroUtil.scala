package com.todesking.scalanb.util

import scala.reflect.macros.blackbox.Context

object MacroUtil {
  def bind(c: Context): Impl[c.type] = new Impl[c.type](c)

  private[this] var treeToSource = Map.empty[String, String]
  private[this] val debug = false

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

    // Sometimes val name contains trailing space. Weird.
    def enclosingOwnerName: String =
      c.internal.enclosingOwner.name.decodedName.toString.replaceAll("""\s+$""", "")

    def register(t: Tree): Unit = {
      // When tree is transformed via annotation macro expantion,
      // the tree's pos mutated from range pos to offset pos.
      // Still unclear who does that.
      // Whatever, I'll save ranged tree and its source, and use it later for range-vanished tree.
      if (t.pos.isRange && !treeToSource.contains(t.toString)) {
        val src = source(t)
        treeToSource = treeToSource + (t.toString -> source(t))
        t.children.foreach(register)
      }
    }

    def wholeSource(trees: Seq[Tree]): String =
      sources(trees).flatMap(_._1).mkString("\n")

    def sources(ts: Seq[Tree]): Seq[(Option[String], Tree)] = {
      import c.universe._
      val initialPos = 0
      // Dirty hack to support `val (a, b) = expr` style statements
      // That statements are desugared to `val x$1 = expr match ...; val a = x._1; val b = x._2`
      ts.foldLeft((initialPos, Seq.empty[(Option[String], Tree)])) {
        case ((pos, trees), t) =>
          val (start, end) = range(t)
          val last = source(t).last
          val cont = end <= pos || (end == pos + 1 && (last == ';' || last == '\n'))
          // This tree may synthetic
          val unitInLast = t match {
            case Literal(Constant(())) => trees.size == ts.size - 1
            case _ => false
          }
          val src = if (cont || unitInLast) None else Some(source(t, start = if (pos == 0) None else Some(pos + 1)))
          if (debug) println(s"Source: tree=$t, src=$src, pos=${t.pos}")
          (math.max(end, pos), trees :+ ((src, t)))
      }._2
    }
    private[this] def rangeUnion(l: (Int, Int), r: (Int, Int)) = (math.min(l._1, r._1), math.max(l._2, r._2))

    private[this] def range(t: Tree): (Int, Int) = {
      val noP = (Int.MaxValue, 0)
      def extract(pos: c.universe.Position) = if (pos == c.universe.NoPosition) noP else (pos.start, pos.end)

      import c.universe._
      t match {
        case TypeTree() =>
          // This is synthetic tree and its pos point other place
          t.children.foldLeft(noP) { (p, t) => rangeUnion(p, range(t)) }
        case t @ ValDef(mods, name, tpt, rhs) =>
          rangeUnion(extract(t.pos), range(rhs))
        case t =>
          val initialP = if (t.pos == c.universe.NoPosition) noP else extract(t.pos)
          val childrenP =
            t.children.foldLeft(initialP) { (p, t) =>
              rangeUnion(p, range(t))
            }
          childrenP
      }
    }

    // Read raw source code from tree(NOTE: *best effort*)
    def source(t: Tree, start: Option[Int] = None): String = {
      if (!t.pos.isRange && treeToSource.contains(t.toString))
        return treeToSource(t.toString)

      def clean(s: String) = s.replaceAll("""^\s+|\s+$""", "")
      def content(t: Tree): Array[Char] = {
        val c = t.pos.source.content
        if (c.nonEmpty) c
        else t.children.map(content).find(_.nonEmpty) getOrElse Array()
      }
      val (start0, end) = range(t)
      val st = start.fold(start0) { s => math.min(start0, s) }
      if (st == Int.MaxValue) "<source unavailable>"
      else {
        val src = clean(content(t).slice(st, end + 1).mkString(""))
        import c.universe._
        t match {
          case ValDef(mod, name, tpt, rhs) =>
            // Hack: Sometimes range of "val a = b" contains only "a = b"
            val re = """^(?:\w+\s+)*val\s.*""".r
            src match {
              case `re`() => src
              case _ =>
                // TODO: support mods
                s"val $src"
            }
          case _ => src
        }
      }
    }
  }
}

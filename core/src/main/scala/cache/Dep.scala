package com.todesking.scalanb.cache

import com.todesking.scalanb.Format
import com.todesking.scalanb.util.MacroUtil

import scala.reflect.macros.blackbox.Context

import scala.language.experimental.macros
import scala.language.implicitConversions

class Dep[A] private (val id: DepID, val unwrapUNSAFE: A) {
  def decompose[B](implicit ev: Decomposable[A, B]): B = ev(this)

  def mapUNSAFE[B](f: A => B): Dep[B] = Dep.buildUNSAFE(id, f(unwrapUNSAFE))

  def map[B](f: A => B): Dep[B] = macro Dep.MacroImpl.mapImpl[A, B]

  def foreach(f: A => Unit): Unit = f(unwrapUNSAFE)
}

object Dep {
  def buildUNSAFE[A](id: DepID, value: A) = new Dep(id, value)
  implicit def fromInt(i: Int): Dep[Int] = buildUNSAFE(DepID(s"int:$i", s"$i", Seq()), i)

  implicit def format[A: Format]: Format[Dep[A]] = Format[Dep[A]] { d =>
    Format.of[A].apply(d.unwrapUNSAFE)
  }

  class MacroImpl(val c: Context) {
    import c.universe.Expr
    import c.universe.WeakTypeTag
    import c.universe.Quasiquote
    val util = MacroUtil.bind[c.type](c)

    def mapImpl[A, B: WeakTypeTag](f: Expr[A => B]): Expr[Dep[B]] = {
      val src = util.stringLiteral(util.source(f.tree))
      val self = c.prefix
      c.Expr[Dep[B]](q"""
      val self = $self
      val id = self.id.map($src)
      _root_.com.todesking.scalanb.cache.Dep.buildUNSAFE(id, $f.apply(self.unwrapUNSAFE))
      """)
    }
  }
}

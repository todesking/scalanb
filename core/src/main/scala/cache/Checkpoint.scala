package com.todesking.scalanb.cache

import scala.reflect.macros.blackbox.Context

import scala.language.experimental.macros

class Checkpoint(val fs: CacheFS) {
  def nocache[R](f: R): Dep[R] = macro Checkpoint.NoCacheImpl.apply[R]
  def cache0[R: Cacheable](f: R): Dep[R] = macro Checkpoint.CacheImpl.apply0[R]
  def cache[A, R: Cacheable](args: DepArg[A])(f: A => R): Dep[R] = macro Checkpoint.CacheImpl.apply1[A, R]

  def unwrap[A](args: DepArg[A])(f: A => Unit): Unit = f(args.value)

  def cacheImpl[A](c: Cacheable[A], id: DepID, value: => A): Dep[A] = {
    c.load(fs, id) getOrElse {
      val dep = Dep.buildUNSAFE(id, value)
      c.save(fs, dep)
      dep
    }
  }
}

object Checkpoint {
  class NoCacheImpl(val c: Context) {
    import c.Expr
    import c.WeakTypeTag
    import c.universe.Quasiquote

    def apply[R: WeakTypeTag](f: Expr[R]): Expr[Dep[R]] = {
      val name = c.internal.enclosingOwner.name.decodedName.toString
      def src = f.tree.toString
      val id = q"_root_.com.todesking.scalanb.cache.DepID($name, $src, _root_.scala.collection.immutable.Seq())"
      Expr[Dep[R]](q"_root_.com.todesking.scalanb.cache.Dep.buildUNSAFE($id, $f)")
    }
  }
  class CacheImpl(val c: Context) {
    import c.Expr
    import c.WeakTypeTag
    import c.universe.Quasiquote

    val valName = c.internal.enclosingOwner.name.decodedName.toString

    def apply0[R: WeakTypeTag](f: Expr[R])(ev: Expr[Cacheable[R]]): Expr[Dep[R]] = {
      def src = f.tree.toString
      val id = q"_root_.com.todesking.scalanb.cache.DepID($valName, $src, _root_.scala.collection.immutable.Seq())"
      Expr[Dep[R]](q"${c.prefix}.cacheImpl($ev, $id, $f)")
    }

    def apply1[A: WeakTypeTag, R: WeakTypeTag](args: Expr[DepArg[A]])(f: Expr[A => R])(ev: Expr[Cacheable[R]]): Expr[Dep[R]] = {
      def src = f.tree.toString
      val id = q"_root_.com.todesking.scalanb.cache.DepID($valName, $src, $args.ids)"
      Expr[Dep[R]](q"${c.prefix}.cacheImpl($ev, $id, $f($args.value))")
    }
  }
}

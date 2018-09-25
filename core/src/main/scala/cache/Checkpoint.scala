package com.todesking.scalanb.cache

import com.todesking.scalanb.util.MacroUtil

import scala.reflect.macros.blackbox.Context

import scala.language.experimental.macros

// eventHandler: (id, cached) => Unit
class Checkpoint(val fs: CacheFS, eventHandler: (CacheFS, DepID, Boolean) => Unit = { (_, _, _) => }) {
  def source[R](f: R): Dep[R] = macro Checkpoint.Impl.nocache0[R]
  def join[A, R](args: DepArg[A])(f: A => R): Dep[R] = macro Checkpoint.Impl.nocache1[A, R]
  def cache0[R: Cacheable](f: R): Dep[R] = macro Checkpoint.Impl.cache0[R]
  def cache[A, R: Cacheable](args: DepArg[A])(f: A => R): Dep[R] = macro Checkpoint.Impl.cache1[A, R]

  def unwrap[A](args: DepArg[A])(f: A => Unit): Unit = f(args.value)

  def cacheImpl[A](c: Cacheable[A], id: DepID, value: => A): Dep[A] = {
    val cached = c.load(fs, id)
    eventHandler(fs, id, cached.nonEmpty)
    cached getOrElse {
      val dep = Dep.buildUNSAFE(id, value)
      c.save(fs, dep)
      dep
    }
  }
}

object Checkpoint {
  class Impl(val c: Context) {
    import c.Expr
    import c.WeakTypeTag
    import c.universe.Quasiquote
    import c.universe.Tree

    val util = MacroUtil.bind[c.type](c)

    val valName = util.enclosingOwnerName

    def nocache0[R: WeakTypeTag](f: Expr[R]): Expr[Dep[R]] =
      nocacheImpl[Unit, R](f.tree.toString, f.tree)

    def nocache1[A: WeakTypeTag, R: WeakTypeTag](args: Expr[DepArg[A]])(f: Expr[R]): Expr[Dep[R]] =
      nocacheImpl[A, R](f.tree.toString, q"$f($args.value)")

    private[this] def nocacheImpl[A: WeakTypeTag, R: WeakTypeTag](src: String, value: Tree): Expr[Dep[R]] = {
      val id = q"_root_.com.todesking.scalanb.cache.DepID($valName, $src, _root_.scala.collection.immutable.Seq())"
      Expr[Dep[R]](q"_root_.com.todesking.scalanb.cache.Dep.buildUNSAFE($id, $value)")
    }

    def cache0[R: WeakTypeTag](f: Expr[R])(ev: Expr[Cacheable[R]]): Expr[Dep[R]] = {
      def src = f.tree.toString
      val id = q"_root_.com.todesking.scalanb.cache.DepID($valName, $src, _root_.scala.collection.immutable.Seq())"
      Expr[Dep[R]](q"${c.prefix}.cacheImpl($ev, $id, $f)")
    }

    def cache1[A: WeakTypeTag, R: WeakTypeTag](args: Expr[DepArg[A]])(f: Expr[A => R])(ev: Expr[Cacheable[R]]): Expr[Dep[R]] = {
      def src = f.tree.toString
      val id = q"_root_.com.todesking.scalanb.cache.DepID($valName, $src, $args.ids)"
      Expr[Dep[R]](q"${c.prefix}.cacheImpl($ev, $id, $f($args.value))")
    }
  }
}

package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.util.MacroUtil
import com.todesking.scalanb.util.Digest

import scala.reflect.macros.blackbox.Context

import scala.language.experimental.macros

class Checkpoint(val fs: FileSystem, eventListener: CacheEventListener = CacheEventListener.Null) {
  def source[R](f: R): Dep[R] = macro Checkpoint.Impl.nocache0[R]
  def join[A, R](args: DepArg[A])(f: A => R): Dep[R] = macro Checkpoint.Impl.nocache1[A, R]
  def cache0[R: Cacheable](f: R): Dep[R] = macro Checkpoint.Impl.cache0[R]
  def cache[A, R: Cacheable](args: DepArg[A])(f: A => R): Dep[R] = macro Checkpoint.Impl.cache1[A, R]

  def unwrap[A](args: DepArg[A])(f: A => Unit): Unit = f(args.value)

  def cacheImpl[A](c: Cacheable[A], id: DepID, value: => A): Dep[A] = {
    val nfs = fs.namespace(Checkpoint.pathString(id))
    val cached = c.load(nfs, "data")
    if (cached.isEmpty) eventListener.miss(fs, id)
    else eventListener.hit(fs, id)
    cached.map { v =>
      Dep.buildUNSAFE(id, v)
    } getOrElse {
      val v = value
      c.save(nfs, "data")(v)
      Dep.buildUNSAFE(id, v)
    }
  }
}

object Checkpoint {
  def pathString(id: DepID): String = {
    s"${id.namespace}/${id.name}/${Digest.hex(id.stringForDigest)}"
  }

  class Impl(val c: Context) {
    import c.Expr
    import c.WeakTypeTag
    import c.universe.Quasiquote
    import c.universe.Tree

    val util = MacroUtil.bind(c)

    val valName = util.enclosingOwnerName

    def nocache0[R: WeakTypeTag](f: Expr[R]): Expr[Dep[R]] =
      nocacheImpl[Unit, R](f.tree.toString, f.tree)

    def nocache1[A: WeakTypeTag, R: WeakTypeTag](args: Expr[DepArg[A]])(f: Expr[R]): Expr[Dep[R]] =
      nocacheImpl[A, R](f.tree.toString, q"$f($args.value)")

    val emptySeq = q"_root_.scala.collection.immutable.Seq()"
    val getClassName = q"this.getClass.getName"

    private[this] def nocacheImpl[A: WeakTypeTag, R: WeakTypeTag](src: String, value: Tree): Expr[Dep[R]] = {
      val id = q"_root_.com.todesking.scalanb.cache.DepID.root($getClassName, $valName, $src, $emptySeq)"
      Expr[Dep[R]](q"_root_.com.todesking.scalanb.cache.Dep.buildUNSAFE($id, $value)")
    }

    def cache0[R: WeakTypeTag](f: Expr[R])(ev: Expr[Cacheable[R]]): Expr[Dep[R]] = {
      def src = f.tree.toString
      val id = q"_root_.com.todesking.scalanb.cache.DepID.root($getClassName, $valName, $src, $emptySeq)"
      Expr[Dep[R]](q"${c.prefix}.cacheImpl($ev, $id, $f)")
    }

    def cache1[A: WeakTypeTag, R: WeakTypeTag](args: Expr[DepArg[A]])(f: Expr[A => R])(ev: Expr[Cacheable[R]]): Expr[Dep[R]] = {
      def src = f.tree.toString
      val id = q""
      Expr[Dep[R]](q"""
        val args = $args
        val id = _root_.com.todesking.scalanb.cache.DepID.root($getClassName, $valName, $src, $args.ids)
        ${c.prefix}.cacheImpl($ev, id, $f(args.value))
      """)
    }
  }
}

package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.util.MacroUtil

import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe.TypeTag

import scala.language.experimental.macros

class CacheContext(val fs: FileSystem, val eventListener: CacheEventListener = CacheEventListener.Null)

class Caching(val className: String) {
  def this(klass: Class[_]) = this(klass.getName)

  def source[R](f: R): Dep[R] = macro Caching.Impl.nocache0[R]
  def join[A, R](args: DepArg[A])(f: A => R): Dep[R] = macro Caching.Impl.nocache1[A, R]
  def unwrap[A](args: DepArg[A])(f: A => Unit): Unit = f(args.value)

  def cache0[R: Cacheable: TypeTag](f: R)(implicit ctx: CacheContext): Dep[R] = macro Caching.Impl.cache0[R]
  def cache[A, R: Cacheable: TypeTag](args: DepArg[A])(f: A => R)(implicit ctx: CacheContext): Dep[R] = macro Caching.Impl.cache1[A, R]

  private[this] def time[A](a: => A): (Long, A) = {
    val start = System.currentTimeMillis()
    val value = a
    val duration = System.currentTimeMillis() - start
    (duration, value)
  }
  def cacheImpl[A](ctx: CacheContext, c: Cacheable[A], tt: TypeTag[A], id: DepID, value: => A): Dep[A] = {
    import ctx.{ fs, eventListener }
    val nfs = fs.namespace(id.pathString)
    if (fs.exists(id.pathString)) {
      val meta = MetaData.fromJson(nfs.readString("cache.json"))
      eventListener.hit(fs, id, meta)
      Dep.lazyUNSAFE(id) {
        val (duration, cached) = time { c.load(nfs, "data") }
        eventListener.loaded(fs, id, meta, duration)
        cached
      }
    } else {
      eventListener.miss(fs, id)
      val (meta, v) = save(c, tt, nfs, id, value)
      eventListener.saved(fs, id, meta)
      Dep.eagerUNSAFE(id, v)
    }
  }
  private[this] def save[A](c: Cacheable[A], tt: TypeTag[A], fs: FileSystem, id: DepID, value: => A): (MetaData, A) = {
    import java.time.Instant
    val start = Instant.now()
    val (calcDuration, v) = time { value }
    val (saveDuration, _) = time { c.save(fs, "data")(v) }
    val meta = MetaData(id, tt.tpe.typeSymbol.name.decodedName.toString, start, calcDuration, saveDuration)
    fs.writeString("cache.json", meta.toJson)
    (meta, v)
  }
}

object Caching {
  class Impl(val c: Context) {
    import c.Expr
    import c.WeakTypeTag
    import c.universe.Quasiquote
    import c.universe.Tree

    val util = MacroUtil.bind(c)

    val valName = util.enclosingOwnerName

    def nocache0[R: WeakTypeTag](f: Expr[R]): Expr[Dep[R]] =
      nocacheImpl[Unit, R](source(f.tree), f.tree, emptySeq)

    def nocache1[A: WeakTypeTag, R: WeakTypeTag](args: Expr[DepArg[A]])(f: Expr[R]): Expr[Dep[R]] =
      nocacheImpl[A, R](source(f.tree), q"$f($args.value)", q"$args.ids")

    val emptySeq = q"_root_.scala.collection.immutable.Seq()"
    val getClassName = q"this.getClass.getName"

    private[this] def nocacheImpl[A: WeakTypeTag, R: WeakTypeTag](src: String, value: Tree, ids: Tree): Expr[Dep[R]] = {
      val id = q"_root_.com.todesking.scalanb.cache.DepID.root($getClassName, $valName, $src, $ids)"
      Expr[Dep[R]](q"_root_.com.todesking.scalanb.cache.Dep.lazyUNSAFE($id)($value)")
    }

    def cache0[R: WeakTypeTag](f: Expr[R])(ev: Expr[Cacheable[R]], tt: Expr[TypeTag[R]], ctx: Expr[CacheContext]): Expr[Dep[R]] = {
      def src = source(f.tree)
      val id = q"_root_.com.todesking.scalanb.cache.DepID.root($getClassName, $valName, $src, $emptySeq)"
      Expr[Dep[R]](q"${c.prefix}.cacheImpl($ctx, $ev, $tt, $id, $f)")
    }

    def cache1[A: WeakTypeTag, R: WeakTypeTag](args: Expr[DepArg[A]])(f: Expr[A => R])(
      ev: Expr[Cacheable[R]], tt: Expr[TypeTag[R]], ctx: Expr[CacheContext]): Expr[Dep[R]] = {
      def src = source(f.tree)
      val id = q""
      Expr[Dep[R]](q"""
        val args = $args
        val id = _root_.com.todesking.scalanb.cache.DepID.root($getClassName, $valName, $src, $args.ids)
        ${c.prefix}.cacheImpl($ctx, $ev, $tt, id, $f(args.value))
      """)
    }

    def source(t: Tree): String = {
      // Hack: If argument has no name, scalac will name it as "x$123".
      // This name will change for each compile(if other part of source changed),
      // so I should "normalize" these names.
      val re = """(x\d*\$\d+)""".r
      val nameMap =
        re.findAllMatchIn(t.toString)
          .map(_.group(0))
          .foldLeft((Map.empty[String, String], 0)) {
            case ((m, i), name) =>
              m.get(name).fold {
                (m + (name -> s"_x${i}_"), i + 1)
              } { _ =>
                (m, i)
              }
          }._1
      re.replaceAllIn(t.toString, { m => nameMap(m.group(0)) })
    }
  }
}

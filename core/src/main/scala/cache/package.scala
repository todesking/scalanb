package com.todesking.scalanb.cache

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import scala.language.implicitConversions

trait IO {
  def protocol: String
  def path(id: DepID): String
  def write(id: DepID, data: Array[Byte]): Unit
  def read(id: DepID): Option[Array[Byte]]
}

object IO {
  def pathString(id: DepID): String = {
    val sha1 = java.security.MessageDigest.getInstance("SHA-1")
    val digest = sha1.digest(id.stringForDigest.getBytes)
    val digestString = digest.map { x => f"$x%02x" }.mkString("")
    s"${id.name}/$digestString"
  }
  class File(basePath: String) extends IO {
    import java.nio.file.{ Files, Paths }
    override def protocol = "file"
    override def path(id: DepID) = {
      s"$basePath/${pathString(id)}"
    }
    override def write(id: DepID, data: Array[Byte]) = {
      Files.write(Paths.get(path(id)), data)
    }
    override def read(id: DepID): Option[Array[Byte]] = {
      val p = Paths.get(path(id))
      if (Files.exists(p)) Some(Files.readAllBytes(p))
      else None
    }
  }
}

trait Cacheable[A] {
  def save(io: IO, d: Dep[A]): Unit
  def load(io: IO, id: DepID): Option[Dep[A]]
}
object Cacheable {
  import java.io.{ ObjectInputStream, ObjectOutputStream }
  def byJavaSerialization[A](doPut: (A, ObjectOutputStream) => Unit)(doGet: ObjectInputStream => A): Cacheable[A] = new Cacheable[A] {
    override def save(io: IO, d: Dep[A]) = {
      val buf = new java.io.ByteArrayOutputStream()
      val os = new java.io.ObjectOutputStream(buf)
      doPut(d.unwrapUNSAFE, os)
      os.close()
      val data = buf.toByteArray
      io.write(d.id, data)
    }

    override def load(io: IO, id: DepID) = {
      io.read(id).map { data =>
        val is = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(data))
        Dep.buildUNSAFE(id, doGet(is))
      }
    }
  }
  // This method should not be implicit:
  // Some Serializables are not suitable for caching(e.g. Spark DataFrame)
  def ofSerializable[A <: Serializable] =
    byJavaSerialization[A] { (x, os) => os.writeObject(x) } { is => is.readObject().asInstanceOf[A] }
  implicit def ofInt: Cacheable[Int] =
    byJavaSerialization[Int] { (x, os) => os.writeInt(x) } { is => is.readInt() }
}

case class DepID(name: String, src: String, deps: Seq[DepID]) {
  override def toString = s"$name[$src] ${if (deps.nonEmpty) s"<- ${deps.mkString(", ")}" else ""}"
  def stringForDigest: String = s"$name[$src] ${if (deps.nonEmpty) s"<- ${deps.map(_.stringForDigest).mkString(", ")}" else ""}"
}

class Dep[A] private (val id: DepID, val unwrapUNSAFE: A) {
}

object Dep {
  def buildUNSAFE[A](id: DepID, value: A) = new Dep(id, value)
  implicit def fromInt(i: Int): Dep[Int] = buildUNSAFE(new DepID(s"int:$i", s"$i", Seq()), i)
}

trait Dependable[A, B] {
  def apply(a: A): Dep[B]
}
object Dependable {
  def apply[A, B](f: A => Dep[B]): Dependable[A, B] = new Dependable[A, B] {
    override def apply(a: A) = f(a)
  }
  implicit def ofDep[A]: Dependable[Dep[A], A] =
    apply(identity)
  implicit val ofInt: Dependable[Int, Int] =
    apply { i => Dep.fromInt(i) }
}

class DepArg[A](val ids: Seq[DepID], val value: A)
object DepArg {
  implicit def fromDep[A, B](x: A)(implicit ev: Dependable[A, B]): DepArg[B] = {
    val dep = ev(x)
    new DepArg(Seq(dep.id), dep.unwrapUNSAFE)
  }
  implicit def fromTuple2[A1, B1, A2, B2](x: (A1, A2))(implicit ev1: Dependable[A1, B1], ev2: Dependable[A2, B2]): DepArg[(B1, B2)] = {
    val a1 = ev1(x._1)
    val a2 = ev2(x._2)
    new DepArg(Seq(a1.id, a2.id), (a1.unwrapUNSAFE, a2.unwrapUNSAFE))
  }
  implicit def fromTuple3[A1, B1, A2, B2, A3, B3](x: (A1, A2, A3))(
    implicit
    ev1: Dependable[A1, B1],
    ev2: Dependable[A2, B2],
    ev3: Dependable[A3, B3]): DepArg[(B1, B2, B3)] = {
    val a1 = ev1(x._1)
    val a2 = ev2(x._2)
    val a3 = ev3(x._3)
    new DepArg(Seq(a1.id, a2.id, a3.id), (a1.unwrapUNSAFE, a2.unwrapUNSAFE, a3.unwrapUNSAFE))
  }
  implicit def fromTuple4[A1, B1, A2, B2, A3, B3, A4, B4](x: (A1, A2, A3, A4))(
    implicit
    ev1: Dependable[A1, B1],
    ev2: Dependable[A2, B2],
    ev3: Dependable[A3, B3],
    ev4: Dependable[A4, B4]): DepArg[(B1, B2, B3, B4)] = {
    val a1 = ev1(x._1)
    val a2 = ev2(x._2)
    val a3 = ev3(x._3)
    val a4 = ev4(x._4)
    new DepArg(Seq(a1.id, a2.id, a3.id, a4.id), (a1.unwrapUNSAFE, a2.unwrapUNSAFE, a3.unwrapUNSAFE, a4.unwrapUNSAFE))
  }
  implicit def fromTuple5[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5](x: (A1, A2, A3, A4, A5))(
    implicit
    ev1: Dependable[A1, B1],
    ev2: Dependable[A2, B2],
    ev3: Dependable[A3, B3],
    ev4: Dependable[A4, B4],
    ev5: Dependable[A5, B5]): DepArg[(B1, B2, B3, B4, B5)] = {
    val a1 = ev1(x._1)
    val a2 = ev2(x._2)
    val a3 = ev3(x._3)
    val a4 = ev4(x._4)
    val a5 = ev5(x._5)
    new DepArg(Seq(a1.id, a2.id, a3.id, a4.id, a5.id), (a1.unwrapUNSAFE, a2.unwrapUNSAFE, a3.unwrapUNSAFE, a4.unwrapUNSAFE, a5.unwrapUNSAFE))
  }
}

class Checkpoint(val io: IO) {
  def nocache[R](f: R): Dep[R] = macro Checkpoint.NoCacheImpl.apply[R]
  def cache0[R: Cacheable](f: R): Dep[R] = macro Checkpoint.CacheImpl.apply0[R]
  def cache[A, R: Cacheable](args: DepArg[A])(f: A => R): Dep[R] = macro Checkpoint.CacheImpl.apply1[A, R]

  def cacheImpl[A](c: Cacheable[A], id: DepID, value: => A): Dep[A] = {
    c.load(io, id) getOrElse {
      val dep = Dep.buildUNSAFE(id, value)
      c.save(io, dep)
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

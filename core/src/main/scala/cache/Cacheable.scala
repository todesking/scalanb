package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem
import scala.reflect.ClassTag

trait Cacheable[A] { self =>
  def save(fs: FileSystem, name: String)(value: A): Unit
  def load(fs: FileSystem, name: String): A

  def transform[B](push: B => A)(pull: A => B): Cacheable[B] = new Cacheable[B] {
    override def save(fs: FileSystem, name: String)(value: B) =
      self.save(fs, name)(push(value))
    override def load(fs: FileSystem, name: String) =
      pull(self.load(fs, name))
  }
}

trait CacheableLowPriority {
  implicit def ofSeqCacheable[A: Cacheable]: Cacheable[Seq[A]] = new Cacheable[Seq[A]] {
    override def save(fs: FileSystem, name: String)(value: Seq[A]) = {
      val nfs = fs.namespace(name)
      Cacheable.of[Int].save(nfs, "size")(value.size)
      value.zipWithIndex.foreach {
        case (v, i) =>
          Cacheable.of[A].save(nfs, s"item_$i")(v)
      }
    }
    override def load(fs: FileSystem, name: String) = {
      val nfs = fs.namespace(name)
      val size = Cacheable.of[Int].load(nfs, "size")
      (0 until size).map { i =>
        Cacheable.of[A].load(nfs, s"item_$i")
      }
    }
  }
}

object Cacheable extends CacheableLowPriority {
  import java.io.{ ObjectInputStream, ObjectOutputStream }

  def of[A: Cacheable]: Cacheable[A] = implicitly[Cacheable[A]]

  def byJavaSerialization[A](doPut: (A, ObjectOutputStream) => Unit)(doGet: ObjectInputStream => A): Cacheable[A] = new Cacheable[A] {
    override def save(fs: FileSystem, name: String)(value: A) = {
      val buf = new java.io.ByteArrayOutputStream()
      val os = new java.io.ObjectOutputStream(buf)
      doPut(value, os)
      os.close()
      val data = buf.toByteArray
      fs.writeBytes(name, data)
    }

    override def load(fs: FileSystem, name: String) = {
      val data = fs.readBytes(name)
      val is = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(data))
      doGet(is)
    }
  }

  // This method should not be implicit:
  // Some Serializables are not suitable for caching(e.g. Spark DataFrame)
  def ofSerializable[A <: java.io.Serializable] =
    byJavaSerialization[A] { (x, os) => os.writeObject(x) } { is => is.readObject().asInstanceOf[A] }

  implicit def ofInt: Cacheable[Int] =
    byJavaSerialization[Int] { (x, os) => os.writeInt(x) } { is => is.readInt() }

  implicit def ofString: Cacheable[String] =
    ofSerializable[String]

  implicit def ofTuple2[A1: Cacheable, A2: Cacheable]: Cacheable[(A1, A2)] = new Cacheable[(A1, A2)] {
    override def save(fs: FileSystem, name: String)(value: (A1, A2)) = {
      val nfs = fs.namespace(name)
      of[A1].save(nfs, "_1")(value._1)
      of[A2].save(nfs, "_2")(value._2)
    }
    override def load(fs: FileSystem, name: String) = {
      val nfs = fs.namespace(name)
      val a1 = of[A1].load(nfs, "_1")
      val a2 = of[A2].load(nfs, "_2")
      (a1, a2)
    }
  }

  implicit def ofTuple3[A1: Cacheable, A2: Cacheable, A3: Cacheable]: Cacheable[(A1, A2, A3)] = new Cacheable[(A1, A2, A3)] {
    override def save(fs: FileSystem, name: String)(value: (A1, A2, A3)) = {
      val nfs = fs.namespace(name)
      of[A1].save(nfs, "_1")(value._1)
      of[A2].save(nfs, "_2")(value._2)
      of[A3].save(nfs, "_3")(value._3)
    }
    override def load(fs: FileSystem, name: String) = {
      val nfs = fs.namespace(name)
      val a1 = of[A1].load(nfs, "_1")
      val a2 = of[A2].load(nfs, "_2")
      val a3 = of[A3].load(nfs, "_3")
      (a1, a2, a3)
    }
  }

  implicit def ofArray[A: ClassTag](implicit ev: Cacheable[Seq[A]]): Cacheable[Array[A]] =
    ev.transform[Array[A]](_.toSeq)(_.toArray)

  implicit def ofIndexedSeq[A](implicit ev: Cacheable[Seq[A]]): Cacheable[IndexedSeq[A]] =
    ev.transform[IndexedSeq[A]](identity)(_.toIndexedSeq)
  implicit def ofImmutableIndexedSeq[A](implicit ev: Cacheable[Seq[A]]): Cacheable[scala.collection.immutable.IndexedSeq[A]] =
    ev.transform[scala.collection.immutable.IndexedSeq[A]](identity)(_.toIndexedSeq)

  private[this] def ofSeqX[A: ClassTag]: Cacheable[Seq[A]] =
    ofSerializable[Array[A]].transform[Seq[A]](_.toArray)(_.toSeq)

  implicit def ofSeqByte: Cacheable[Seq[Byte]] = ofSeqX[Byte]
  implicit def ofSeqShort: Cacheable[Seq[Short]] = ofSeqX[Short]
  implicit def ofSeqInt: Cacheable[Seq[Int]] = ofSeqX[Int]
  implicit def ofSeqLong: Cacheable[Seq[Long]] = ofSeqX[Long]
  implicit def ofSeqFloat: Cacheable[Seq[Float]] = ofSeqX[Float]
  implicit def ofSeqDouble: Cacheable[Seq[Double]] = ofSeqX[Double]
  implicit def ofSeqChar: Cacheable[Seq[Char]] = ofSeqX[Char]
  implicit def ofSeqString: Cacheable[Seq[String]] = ofSeqX[String]
}

package com.todesking.scalanb.cache

import com.todesking.scalanb.io.FileSystem
import scala.reflect.ClassTag

trait Cacheable[A] { self =>
  def save(fs: FileSystem, name: String)(value: A): Unit
  def load(fs: FileSystem, name: String): Option[A]

  def transform[B](push: B => A)(pull: A => B): Cacheable[B] = new Cacheable[B] {
    override def save(fs: FileSystem, name: String)(value: B) =
      self.save(fs, name)(push(value))
    override def load(fs: FileSystem, name: String) =
      self.load(fs, name).map(pull)
  }
}

trait CacheableLowPriority {
  protected def cacheable[A: Cacheable]: Cacheable[A] = implicitly[Cacheable[A]]

  implicit def ofArrayCacheable[A: Cacheable: ClassTag]: Cacheable[Array[A]] =
    ofSeqCacheable[A].transform[Array[A]](_.toSeq)(_.toArray)

  implicit def ofSeqCacheable[A: Cacheable]: Cacheable[Seq[A]] = new Cacheable[Seq[A]] {
    override def save(fs: FileSystem, name: String)(value: Seq[A]) = {
      val nfs = fs.namespace(name)
      cacheable[Int].save(nfs, "size")(value.size)
      value.zipWithIndex.foreach {
        case (v, i) =>
          cacheable[A].save(nfs, s"item_$i")(v)
      }
    }
    override def load(fs: FileSystem, name: String) = {
      val nfs = fs.namespace(name)
      cacheable[Int].load(nfs, "size").flatMap { size =>
        val cached: Seq[Option[A]] =
          (0 until size).map { i =>
            cacheable[A].load(nfs, s"item_$i")
          }
        Some(cached.map(_.get))
      }
    }
  }
}

object Cacheable extends CacheableLowPriority {
  import java.io.{ ObjectInputStream, ObjectOutputStream }
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
      if (!fs.exists(name)) None
      else {
        val data = fs.readBytes(name)
        val is = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(data))
        Some(doGet(is))
      }
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
      cacheable[A1].save(nfs, "_1")(value._1)
      cacheable[A2].save(nfs, "_2")(value._2)
    }
    override def load(fs: FileSystem, name: String) = {
      val nfs = fs.namespace(name)
      for {
        a1 <- cacheable[A1].load(nfs, "_1")
        a2 <- cacheable[A2].load(nfs, "_2")
      } yield (a1, a2)
    }
  }

  implicit def ofTuple3[A1: Cacheable, A2: Cacheable, A3: Cacheable]: Cacheable[(A1, A2, A3)] = new Cacheable[(A1, A2, A3)] {
    override def save(fs: FileSystem, name: String)(value: (A1, A2, A3)) = {
      val nfs = fs.namespace(name)
      cacheable[A1].save(nfs, "_1")(value._1)
      cacheable[A2].save(nfs, "_2")(value._2)
      cacheable[A3].save(nfs, "_3")(value._3)
    }
    override def load(fs: FileSystem, name: String) = {
      val nfs = fs.namespace(name)
      for {
        a1 <- cacheable[A1].load(nfs, "_1")
        a2 <- cacheable[A2].load(nfs, "_2")
        a3 <- cacheable[A3].load(nfs, "_3")
      } yield (a1, a2, a3)
    }
  }

  implicit def ofSeq[A: ClassTag](implicit ev: Cacheable[Array[A]]): Cacheable[Seq[A]] =
    ev.transform[Seq[A]](_.toArray)(_.toSeq)

  implicit def ofArrayInt: Cacheable[Array[Int]] =
    ofSerializable[Array[Int]]
}

package com.todesking.scalanb.cache

import scala.reflect.ClassTag

trait Cacheable[A] { self =>
  def save(fs: CacheFS, d: Dep[A]): Unit
  def load(fs: CacheFS, id: DepID): Option[Dep[A]]

  def transform[B](push: B => A)(pull: A => B): Cacheable[B] = new Cacheable[B] {
    override def save(fs: CacheFS, d: Dep[B]) = {
      self.save(fs, d.mapUNSAFE(push))
    }
    override def load(fs: CacheFS, id: DepID) =
      self.load(fs, id).map(_.mapUNSAFE(pull))
  }
}

trait CacheableLowPriority {
  protected def cacheable[A: Cacheable]: Cacheable[A] = implicitly[Cacheable[A]]

  implicit def ofArrayCacheable[A: Cacheable: ClassTag]: Cacheable[Array[A]] =
    ofSeqCacheable[A].transform[Array[A]](_.toSeq)(_.toArray)

  implicit def ofSeqCacheable[A: Cacheable]: Cacheable[Seq[A]] = new Cacheable[Seq[A]] {
    override def save(fs: CacheFS, d: Dep[Seq[A]]) = {
      val a = d.unwrapUNSAFE
      val ids = Decomposable.ofSeq[A].ids(d.id, a.size)
      cacheable[Int].save(fs, Dep.buildUNSAFE(d.id.item("size"), a.size))
      ids.zip(a).foreach {
        case (id, v) =>
          cacheable[A].save(fs, Dep.buildUNSAFE(id, v))
      }
    }
    override def load(fs: CacheFS, id: DepID) = {
      cacheable[Int].load(fs, id.item("size")).map(_.unwrapUNSAFE).flatMap { size =>
        val cached: Seq[Option[Dep[A]]] =
          (0 until size).map { i =>
            cacheable[A].load(fs, id.item(s"$i"))
          }
        if (cached.exists(_.isEmpty)) {
          None
        } else {
          Some(Dep.buildUNSAFE(id, cached.map(_.get.unwrapUNSAFE)))
        }
      }
    }
  }
}

object Cacheable extends CacheableLowPriority {
  import java.io.{ ObjectInputStream, ObjectOutputStream }
  def byJavaSerialization[A](doPut: (A, ObjectOutputStream) => Unit)(doGet: ObjectInputStream => A): Cacheable[A] = new Cacheable[A] {
    override def save(fs: CacheFS, d: Dep[A]) = {
      val buf = new java.io.ByteArrayOutputStream()
      val os = new java.io.ObjectOutputStream(buf)
      doPut(d.unwrapUNSAFE, os)
      os.close()
      val data = buf.toByteArray
      fs.write(d.id, data)
    }

    override def load(fs: CacheFS, id: DepID) = {
      fs.read(id).map { data =>
        val is = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(data))
        Dep.buildUNSAFE(id, doGet(is))
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
    override def save(fs: CacheFS, d: Dep[(A1, A2)]) = {
      val (a1, a2) = d.decompose
      implicitly[Cacheable[A1]].save(fs, a1)
      implicitly[Cacheable[A2]].save(fs, a2)
    }
    override def load(fs: CacheFS, id: DepID) = {
      val (i1, i2) = Decomposable.ofTuple2[A1, A2].ids(id)
      for {
        a1 <- implicitly[Cacheable[A1]].load(fs, i1)
        a2 <- implicitly[Cacheable[A2]].load(fs, i2)
      } yield Dep.buildUNSAFE(id, (a1.unwrapUNSAFE, a2.unwrapUNSAFE))
    }
  }

  implicit def ofTuple3[A1: Cacheable, A2: Cacheable, A3: Cacheable]: Cacheable[(A1, A2, A3)] = new Cacheable[(A1, A2, A3)] {
    override def save(fs: CacheFS, d: Dep[(A1, A2, A3)]) = {
      val (a1, a2, a3) = d.decompose
      implicitly[Cacheable[A1]].save(fs, a1)
      implicitly[Cacheable[A2]].save(fs, a2)
      implicitly[Cacheable[A3]].save(fs, a3)
    }
    override def load(fs: CacheFS, id: DepID) = {
      val (i1, i2, i3) = Decomposable.ofTuple3[A1, A2, A3].ids(id)
      for {
        a1 <- implicitly[Cacheable[A1]].load(fs, i1)
        a2 <- implicitly[Cacheable[A2]].load(fs, i2)
        a3 <- implicitly[Cacheable[A3]].load(fs, i3)
      } yield Dep.buildUNSAFE(id, (a1.unwrapUNSAFE, a2.unwrapUNSAFE, a3.unwrapUNSAFE))
    }
  }

  implicit def ofSeq[A: ClassTag](implicit ev: Cacheable[Array[A]]): Cacheable[Seq[A]] =
    ev.transform[Seq[A]](_.toArray)(_.toSeq)

  implicit def ofArrayInt: Cacheable[Array[Int]] =
    ofSerializable[Array[Int]]
}

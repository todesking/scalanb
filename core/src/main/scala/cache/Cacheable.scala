package com.todesking.scalanb.cache

trait Cacheable[A] {
  def save(fs: CacheFS, d: Dep[A]): Unit
  def load(fs: CacheFS, id: DepID): Option[Dep[A]]
}
object Cacheable {
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
  def ofSerializable[A <: Serializable] =
    byJavaSerialization[A] { (x, os) => os.writeObject(x) } { is => is.readObject().asInstanceOf[A] }

  implicit def ofInt: Cacheable[Int] =
    byJavaSerialization[Int] { (x, os) => os.writeInt(x) } { is => is.readInt() }

  implicit def ofTuple2[A1: Cacheable, A2: Cacheable]: Cacheable[(A1, A2)] = new Cacheable[(A1, A2)] {
    override def save(fs: CacheFS, d: Dep[(A1, A2)]) = {
      val (a1, a2) = d.decompose
      implicitly[Cacheable[A1]].save(fs, a1)
      implicitly[Cacheable[A2]].save(fs, a2)
    }
    override def load(fs: CacheFS, id: DepID) = {
      val (i1, i2) = Decomposable.ofTuple2[A1, A2].applyID(id)
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
      val (i1, i2, i3) = Decomposable.ofTuple3[A1, A2, A3].applyID(id)
      for {
        a1 <- implicitly[Cacheable[A1]].load(fs, i1)
        a2 <- implicitly[Cacheable[A2]].load(fs, i2)
        a3 <- implicitly[Cacheable[A3]].load(fs, i3)
      } yield Dep.buildUNSAFE(id, (a1.unwrapUNSAFE, a2.unwrapUNSAFE, a3.unwrapUNSAFE))
    }
  }
}

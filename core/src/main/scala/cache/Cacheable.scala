package com.todesking.scalanb.cache

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

package com.todesking.scalanb.cache

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
    apply { i => Dep.buildUNSAFE(DepID(s"int.$i", s"$i", Seq()), i) }
  implicit def ofSeqInt: Dependable[Seq[Int], Seq[Int]] =
    apply { a =>
      val id = DepID.Root("seq.int", s"Seq(${a.mkString(",")})", Seq())
      Dep.buildUNSAFE(id, a)
    }
  implicit def ofOption[A, B](implicit ev: Dependable[A, B]): Dependable[Option[A], Option[B]] =
    apply {
      case Some(a) =>
        val d = implicitly[Dependable[A, B]].apply(a)
        val id = DepID.Root(s"option.Some(${d.id.name})", s"Some(${d.id.name})", Seq(d.id))
        Dep.buildUNSAFE(id, Some(d.unwrapUNSAFE))
      case None =>
        Dep.buildUNSAFE(
          DepID.Root("option.None", "None", Seq()),
          None)
    }
}

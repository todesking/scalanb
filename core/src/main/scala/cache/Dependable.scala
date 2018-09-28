package com.todesking.scalanb.cache

trait Dependable[A, B] {
  def apply(a: A): Dep[B]
}
object Dependable {
  def apply[A](f: A => DepID): Dependable[A, A] = new Dependable[A, A] {
    override def apply(a: A) = Dep.buildUNSAFE(f(a), a)
  }

  implicit def ofDep[A]: Dependable[Dep[A], A] = new Dependable[Dep[A], A] {
    override def apply(a: Dep[A]) = a
  }

  implicit val ofInt: Dependable[Int, Int] =
    apply { i => DepID.forValue(s"int.$i", s"$i", Seq()) }

  implicit def ofSeqInt: Dependable[Seq[Int], Seq[Int]] =
    apply { a =>
      DepID.forValue("Seq[Int]", s"Seq(${a.mkString(",")})", Seq())
    }

  // TODO: be more precise
  implicit val ofDouble: Dependable[Double, Double] =
    apply { d => DepID.forValue(s"double.$d", s"$d", Seq()) }

  implicit def ofOption[A, B](implicit ev: Dependable[A, B]): Dependable[Option[A], Option[B]] = new Dependable[Option[A], Option[B]] {
    override def apply(x: Option[A]) = x match {
      case Some(a) =>
        val d = implicitly[Dependable[A, B]].apply(a)
        val id = DepID.forValue(s"option.Some(${d.id.name})", s"Some(${d.id.name})", Seq(d.id))
        Dep.buildUNSAFE(id, Some(d.unwrapUNSAFE))
      case None =>
        Dep.buildUNSAFE(DepID.forValue("option.None", "None", Seq()), None)
    }
  }

  implicit def ofSeq[A, B](implicit ev: Dependable[A, B]): Dependable[Seq[A], Seq[B]] = new Dependable[Seq[A], Seq[B]] {
    override def apply(xs: Seq[A]) = {
      val ds = xs.map { x => implicitly[Dependable[A, B]].apply(x) }
      val name = ds.map(_.id.name).mkString(",")
      val id = DepID.forValue(s"Seq($name)", s"Seq($name)", ds.map(_.id))
      Dep.buildUNSAFE(id, ds.map(_.unwrapUNSAFE))
    }
  }
}

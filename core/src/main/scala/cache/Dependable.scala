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
    apply { i => Dep.fromInt(i) }
}

package com.todesking.scalanb.cache

trait Decomposable[A, B, C] {
  def apply(a: Dep[A]): B
  def applyID(id: DepID): C
}
object Decomposable {
  def apply[A, B, C](f: DepID => C)(g: (C, A) => B): Decomposable[A, B, C] = new Decomposable[A, B, C] {
    override def applyID(id: DepID) = f(id)
    override def apply(a: Dep[A]) = g(applyID(a.id), a.unwrapUNSAFE)
  }
  implicit def ofTuple2[A1, A2]: Decomposable[(A1, A2), (Dep[A1], Dep[A2]), (DepID, DepID)] = apply { id =>
    (id.item("_1"), id.item("_2"))
  } {
    case ((id1, id2), (a1, a2)) =>
      (Dep.buildUNSAFE(id1, a1), Dep.buildUNSAFE(id2, a2))
  }
  implicit def ofTuple3[A1, A2, A3]: Decomposable[(A1, A2, A3), (Dep[A1], Dep[A2], Dep[A3]), (DepID, DepID, DepID)] = apply { id =>
    (id.item("_1"), id.item("_2"), id.item("_3"))
  } {
    case ((id1, id2, id3), (a1, a2, a3)) =>
      (Dep.buildUNSAFE(id1, a1), Dep.buildUNSAFE(id2, a2), Dep.buildUNSAFE(id3, a3))
  }
}

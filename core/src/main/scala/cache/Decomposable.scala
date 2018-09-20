package com.todesking.scalanb.cache

trait Decomposable[A, B] {
  def apply(a: Dep[A]): B
}
object Decomposable {
  class OfTuple2[A1, A2] extends Decomposable[(A1, A2), (Dep[A1], Dep[A2])] {
    def ids(id: DepID): (DepID, DepID) =
      (id.item("_1"), id.item("_2"))
    override def apply(d: Dep[(A1, A2)]) = {
      val (id1, id2) = ids(d.id)
      val (a1, a2) = d.unwrapUNSAFE
      (Dep.buildUNSAFE(id1, a1), Dep.buildUNSAFE(id2, a2))
    }
  }
  implicit def ofTuple2[A1, A2]: OfTuple2[A1, A2] = new OfTuple2[A1, A2]

  class OfTuple3[A1, A2, A3] extends Decomposable[(A1, A2, A3), (Dep[A1], Dep[A2], Dep[A3])] {
    def ids(id: DepID): (DepID, DepID, DepID) =
      (id.item("_1"), id.item("_2"), id.item("_3"))
    override def apply(d: Dep[(A1, A2, A3)]) = {
      val (id1, id2, id3) = ids(d.id)
      val (a1, a2, a3) = d.unwrapUNSAFE
      (Dep.buildUNSAFE(id1, a1), Dep.buildUNSAFE(id2, a2), Dep.buildUNSAFE(id3, a3))
    }
  }
  implicit def ofTuple3[A1, A2, A3]: OfTuple3[A1, A2, A3] = new OfTuple3[A1, A2, A3]

  class OfArray[A: OfSeq] extends Decomposable[Array[A], Array[Dep[A]]] {
    val seq = implicitly[OfSeq[A]]
    def ids(id: DepID, size: Int): Array[DepID] = seq.ids(id, size).toArray
    override def apply(d: Dep[Array[A]]): Array[Dep[A]] =
      seq.apply(Dep.buildUNSAFE(d.id, d.unwrapUNSAFE.toSeq)).toArray
  }
  implicit def ofArray[A: OfSeq]: OfArray[A] = new OfArray[A]

  class OfSeq[A] extends Decomposable[Seq[A], Seq[Dep[A]]] {
    def ids(id: DepID, size: Int): Seq[DepID] =
      (0 until size).map { i => id.item(s"$i") }.toSeq
    override def apply(d: Dep[Seq[A]]): Seq[Dep[A]] = {
      val v = d.unwrapUNSAFE
      ids(d.id, v.size).zip(v).map {
        case (i, x) =>
          Dep.buildUNSAFE(i, x)
      }
    }
  }
  implicit def ofSeq[A]: OfSeq[A] = new OfSeq[A]
}

package com.todesking.scalanb.cache

class Dep[A] private (val id: DepID, val unwrapUNSAFE: A) {
}

object Dep {
  def buildUNSAFE[A](id: DepID, value: A) = new Dep(id, value)
  implicit def fromInt(i: Int): Dep[Int] = buildUNSAFE(new DepID(s"int:$i", s"$i", Seq()), i)
}

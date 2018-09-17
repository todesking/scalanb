package com.todesking.scalanb.cache

import com.todesking.scalanb.Format

import scala.language.implicitConversions

class Dep[A] private (val id: DepID, val unwrapUNSAFE: A) {
}

object Dep {
  def buildUNSAFE[A](id: DepID, value: A) = new Dep(id, value)
  implicit def fromInt(i: Int): Dep[Int] = buildUNSAFE(new DepID(s"int:$i", s"$i", Seq()), i)

  implicit def format[A: Format]: Format[Dep[A]] = Format[Dep[A]] { d =>
    implicitly[Format[A]].apply(d.unwrapUNSAFE)
  }
}

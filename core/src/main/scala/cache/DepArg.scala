package com.todesking.scalanb.cache

import com.todesking.scalanb.impl

import scala.language.implicitConversions

class DepArg[A](val ids: Seq[DepID], calc: => A) {
  lazy val value: A = calc
}
object DepArg extends impl.cache.DepArgTupleInstances {
  implicit def fromDep[A, B](x: A)(implicit ev: Dependable[A, B]): DepArg[B] = {
    val dep = ev(x)
    new DepArg(Seq(dep.id), dep.unwrapUNSAFE)
  }
}

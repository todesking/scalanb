package com.todesking.scalanb.cache

import com.todesking.scalanb.impl

import scala.language.implicitConversions

class DepArg[A](val ids: Seq[DepID], val value: A)
object DepArg extends impl.cache.DepArgTupleInstances {
  implicit def fromDep[A, B](x: A)(implicit ev: Dependable[A, B]): DepArg[B] = {
    val dep = ev(x)
    new DepArg(Seq(dep.id), dep.unwrapUNSAFE)
  }
}

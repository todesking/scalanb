package com.todesking.scalanb.cache

import scala.language.implicitConversions

class DepArg[A](val ids: Seq[DepID], val value: A)
object DepArg {
  implicit def fromDep[A, B](x: A)(implicit ev: Dependable[A, B]): DepArg[B] = {
    val dep = ev(x)
    new DepArg(Seq(dep.id), dep.unwrapUNSAFE)
  }
  // TODO: auto generate
  implicit def fromTuple2[A1, B1, A2, B2](x: (A1, A2))(implicit ev1: Dependable[A1, B1], ev2: Dependable[A2, B2]): DepArg[(B1, B2)] = {
    val a1 = ev1(x._1)
    val a2 = ev2(x._2)
    new DepArg(Seq(a1.id, a2.id), (a1.unwrapUNSAFE, a2.unwrapUNSAFE))
  }
  implicit def fromTuple3[A1, B1, A2, B2, A3, B3](x: (A1, A2, A3))(
    implicit
    ev1: Dependable[A1, B1],
    ev2: Dependable[A2, B2],
    ev3: Dependable[A3, B3]): DepArg[(B1, B2, B3)] = {
    val a1 = ev1(x._1)
    val a2 = ev2(x._2)
    val a3 = ev3(x._3)
    new DepArg(Seq(a1.id, a2.id, a3.id), (a1.unwrapUNSAFE, a2.unwrapUNSAFE, a3.unwrapUNSAFE))
  }
  implicit def fromTuple4[A1, B1, A2, B2, A3, B3, A4, B4](x: (A1, A2, A3, A4))(
    implicit
    ev1: Dependable[A1, B1],
    ev2: Dependable[A2, B2],
    ev3: Dependable[A3, B3],
    ev4: Dependable[A4, B4]): DepArg[(B1, B2, B3, B4)] = {
    val a1 = ev1(x._1)
    val a2 = ev2(x._2)
    val a3 = ev3(x._3)
    val a4 = ev4(x._4)
    new DepArg(Seq(a1.id, a2.id, a3.id, a4.id), (a1.unwrapUNSAFE, a2.unwrapUNSAFE, a3.unwrapUNSAFE, a4.unwrapUNSAFE))
  }
  implicit def fromTuple5[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5](x: (A1, A2, A3, A4, A5))(
    implicit
    ev1: Dependable[A1, B1],
    ev2: Dependable[A2, B2],
    ev3: Dependable[A3, B3],
    ev4: Dependable[A4, B4],
    ev5: Dependable[A5, B5]): DepArg[(B1, B2, B3, B4, B5)] = {
    val a1 = ev1(x._1)
    val a2 = ev2(x._2)
    val a3 = ev3(x._3)
    val a4 = ev4(x._4)
    val a5 = ev5(x._5)
    new DepArg(Seq(a1.id, a2.id, a3.id, a4.id, a5.id), (a1.unwrapUNSAFE, a2.unwrapUNSAFE, a3.unwrapUNSAFE, a4.unwrapUNSAFE, a5.unwrapUNSAFE))
  }
}

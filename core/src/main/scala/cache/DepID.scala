package com.todesking.scalanb.cache

case class DepID(name: String, src: String, deps: Seq[DepID]) {
  override def toString = s"$name[$src] ${if (deps.nonEmpty) s"<- ${deps.mkString(", ")}" else ""}"
  def stringForDigest: String = s"$name[$src] ${if (deps.nonEmpty) s"<- ${deps.map(_.stringForDigest).mkString(", ")}" else ""}"
}

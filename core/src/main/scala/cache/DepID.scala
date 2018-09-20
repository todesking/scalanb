package com.todesking.scalanb.cache

import com.todesking.scalanb.util.Digest

sealed abstract class DepID {
  def name: String
  def deps: Seq[DepID]
  def stringForDigest: String
  def path: Seq[String]
  def item(i: String): DepID.Item =
    DepID.Item(this, i)
  def map(body: String): DepID.Map =
    DepID.Map(this, body)
  override def toString = stringForDigest
}

object DepID {
  def apply(name: String, src: String, deps: Seq[DepID]): Root = Root(name, src, deps)

  case class Root(override val name: String, src: String, override val deps: Seq[DepID]) extends DepID {
    def stringForDigest: String =
      s"$name[$src] ${if (deps.nonEmpty) s"<- { ${deps.map(_.stringForDigest).mkString(", ")} }" else ""}"
    override def path = Seq(name)
  }

  case class Item(parent: DepID, index: String) extends DepID {
    override def name = s"${parent.name}.$index"
    override def deps = Seq(parent)
    override def stringForDigest = s"(${parent.stringForDigest}).$index"
    override def path = parent.path :+ index
  }

  case class Map(parent: DepID, src: String) extends DepID {
    override def name = s"${parent.name}.map($src)"
    override def deps = Seq(parent)
    override def stringForDigest = s"(${parent.stringForDigest}).map($src)"
    override def path = parent.path :+ s"map_${Digest.hex(src)}"
  }
}

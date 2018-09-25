package com.todesking.scalanb.cache

import com.todesking.scalanb.util.Digest

sealed abstract class DepID {
  def namespace: String
  def name: String
  def deps: Seq[DepID]
  def stringForDigest: String
  def item(i: String): DepID.Item =
    DepID.Item(this, i)
  def map(body: String): DepID.Map =
    DepID.Map(this, body)
  override def toString = stringForDigest
  def shortString: String = s"$name/${Digest.hex(stringForDigest)}"
}

object DepID {
  // Ref: https://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-common/filesystem/model.html
  // Also added common bad characters
  private[this] val badChars = """.*[\/:?<>*|"$\u0000-\u001f]+.*""".r
  private[this] def requireFileNameSafe(s: String): Unit = s match {
    case "" | ".." | "." | "/" =>
      throw new RuntimeException(s"Name $s is not allowed")
    case `badChars`() =>
      throw new RuntimeException(s"Name $s is not allowed")
    case _ =>
  }

  private[this] def sanitize(s: String) =
    s.replaceAll("""\$""", "__")

  def forValue(name: String, src: String, deps: Seq[DepID]): Root =
    Root("_values_", sanitize(name), src, deps)

  def root(className: String, name: String, src: String, deps: Seq[DepID]): Root =
    Root(sanitize(className), sanitize(name), src, deps)

  case class Root(override val namespace: String, override val name: String, src: String, override val deps: Seq[DepID]) extends DepID {
    requireFileNameSafe(namespace)
    requireFileNameSafe(name)
    def stringForDigest: String =
      s"$name[$src] ${if (deps.nonEmpty) s"<- { ${deps.map(_.stringForDigest).mkString(", ")} }" else ""}"
  }

  case class Item(parent: DepID, index: String) extends DepID {
    requireFileNameSafe(index)
    override def namespace = parent.namespace
    override def name = s"${parent.name}.$index"
    override def deps = Seq(parent)
    override def stringForDigest = s"(${parent.stringForDigest}).$index"
  }

  case class Map(parent: DepID, src: String) extends DepID {
    override def namespace = parent.namespace
    override def name = s"${parent.name}.map_${Digest.hex(stringForDigest, 8)}"
    override def deps = Seq(parent)
    override def stringForDigest = s"(${parent.stringForDigest}).map($src)"
  }
}

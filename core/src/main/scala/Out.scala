package com.todesking.scalanb

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.io.Writer

trait Out {
  def prepare(): Unit
  def notebook(name: String, ast: ipynb.Notebook): String
  def openLog(name: String): Writer
}

class MultiOut(outs: Seq[Out]) extends Out {
  override def prepare() = outs.foreach(_.prepare())
  override def notebook(name: String, ast: ipynb.Notebook) =
    outs.map(_.notebook(name, ast)).mkString(", ")
  override def openLog(name: String) = {
    // TODO: better error handling
    val children = outs.map(_.openLog(name))
    new Writer {
      override def write(cbuf: Array[Char], off: Int, len: Int) =
        children.foreach(_.write(cbuf, off, len))
      override def close() = children.foreach(_.close())
      override def flush() = children.foreach(_.flush())
    }
  }
}

trait OutFactory {
  def name: String
  def newOut(args: Map[String, String]): Out
}

class FileOutFactory extends OutFactory {
  private[this] val fs = java.nio.file.FileSystems.getDefault
  val defaultPath = fs.getPath(sys.props("user.home"), ".scalanb", "hist")

  override val name = "file"
  override def newOut(args: Map[String, String]): Out = {
    val path = args.get("path").map(Paths.get(_)) getOrElse defaultPath
    new FileOut(path)
  }
}

class FileOut(val path: Path) extends Out {
  override def prepare(): Unit = {
    val _ = path.toFile.mkdirs()
  }

  override def notebook(name: String, ast: ipynb.Notebook): String = {
    import scala.collection.JavaConverters._
    val src = ipynb.JsonMapping.toJson(ast, pretty = true)
    prepare()
    val filePath = path.resolve(s"$name.ipynb")
    val _ = Files.write(filePath, Seq(src).asJava)
    filePath.toString
  }

  override def openLog(name: String) =
    Files.newBufferedWriter(path.resolve(s"$name.log"))
}

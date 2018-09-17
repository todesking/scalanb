package com.todesking.scalanb

import java.io.Writer

import com.todesking.scalanb.io.FileSystem

trait Out {
  def prepare(): Unit

  final def notebook(name: String, ast: ipynb.Notebook): Unit = {
    val src = ipynb.JsonMapping.toJson(ast, pretty = true)
    prepare()
    write(s"$name.ipynb", src)
  }

  final def openLog(name: String): Writer = newWriter(s"$name.log")

  def write(fileName: String, content: String) = {
    val w = newWriter(fileName)
    try { w.write(content.toCharArray) }
    finally { w.close() }
  }

  def newWriter(fileName: String): Writer
}

class MultiOut(outs: Seq[Out]) extends Out {
  override def prepare() = outs.foreach(_.prepare())

  override def newWriter(name: String) = {
    // TODO: better error handling
    val children = outs.map(_.newWriter(name))
    new Writer {
      override def write(cbuf: Array[Char], off: Int, len: Int) =
        children.foreach(_.write(cbuf, off, len))
      override def close() = children.foreach(_.close())
      override def flush() = children.foreach(_.flush())
    }
  }
}

class FSOut(val fs: FileSystem) extends Out {
  override def prepare(): Unit = {
    val _ = fs.prepare()
  }

  override def newWriter(name: String) =
    fs.newWriter(name)
}

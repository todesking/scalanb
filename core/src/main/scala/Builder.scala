package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Cell
import com.todesking.scalanb.ipynb.Output

import play.api.libs.json

trait Builder {
  def setShowTimeMillis(l: Long): Unit

  def code(src: String): Unit

  def markdown(src: String): Unit

  def quiet(): Unit

  def expr(value: Unit): Unit = {}
  def expr(value: Nothing): Unit = throw new AssertionError

  def expr[A: Format](value: A): Unit =
    expr(implicitly[Format[A]].apply(value))

  def expr(value: Value): Unit

  def error(t: Throwable)(implicit format: ErrorFormat): Unit

  def stdout(s: String): Unit
  def stderr(s: String): Unit
  def display(v: Value): Unit

  def finish(): Unit = {}
}

object Builder {
  sealed abstract class Content
  object Content {
    case class StdOut(value: String) extends Content
    case class StdErr(value: String) extends Content
    case class Display(value: Value) extends Content
  }
  case class ExecLog(
    code: String,
    startAt: Long,
    contents: Seq[Content]) {
    def content(c: Content) = copy(contents = contents :+ c)
  }
  object ExecLog {
    def apply(code: String, startAt: Long): ExecLog = ExecLog(code, startAt, Seq())
  }

  def makeExecutionTime(millis: Long): Value = {
    val sec = millis / 1000.0
    var text = f"$sec%.2f[Sec]"
    if (sec > 120) text = text + f" = ${sec / 60}%.2f[Min]"
    if (sec > 7200) text = text + f" = ${sec / 60 / 60}%.2f[Hour]"
    Value.text(f"Execution time: $text")
  }

  class Ipynb extends Builder {
    private[this] var _executionCount = 1
    def executionCount = _executionCount

    private[this] var cells = Seq.empty[Cell]

    private[this] var execLogs = Seq.empty[ExecLog]
    private[this] var currentExecLog: Option[ExecLog] = None

    var _showTimeMillis: Long = 5 * 1000L
    def showTimeMillis = _showTimeMillis
    override def setShowTimeMillis(l: Long) = {
      _showTimeMillis = l
    }

    private[this] def addCell(c: Cell) = {
      this.cells = this.cells :+ c
    }

    override def expr(value: Value) =
      flush(Some(Output.ExecuteResult(value.data, Map(), executionCount)))

    override def error(t: Throwable)(implicit format: ErrorFormat) =
      flush(Some(format.apply(t)))

    override def quiet() = {
      this.currentExecLog = None
    }

    override def markdown(src: String): Unit = {
      flush(None)
      addCell(Cell.Markdown(src))
    }

    override def code(s: String) = {
      currentExecLog.foreach { el =>
        val duration = System.currentTimeMillis() - el.startAt
        if (duration > showTimeMillis) {
          // First, flush previous exec logs
          this.currentExecLog = None
          flush(None)
          // Then flush current one
          this.currentExecLog = Some(el)
          flush(None)
        } else {
          this.execLogs = this.execLogs :+ el
        }
      }
      this.currentExecLog = Some(ExecLog(s, System.currentTimeMillis()))
    }

    override def stdout(s: String) = {
      this.currentExecLog = currentExecLog.map(_.content(Content.StdOut(s)))
    }

    override def stderr(s: String) = {
      this.currentExecLog = currentExecLog.map(_.content(Content.StdErr(s)))
    }

    override def display(v: Value) = {
      this.currentExecLog = currentExecLog.map(_.content(Content.Display(v)))
    }

    private[this] def flushCell(els: Seq[ExecLog], res: Seq[Output]): Unit = {
      def nl(s: String) = if (s.nonEmpty && s.last != '\n') s + "\n" else s
      var outputs = Seq.empty[Output]
      els.foreach { el =>
        outputs = outputs ++ el.contents.map {
          case Content.StdOut(s) =>
            Output.Stream("stdout", nl(s))
          case Content.StdErr(s) =>
            Output.Stream("stderr", nl(s))
          case Content.Display(v) =>
            Output.DisplayData(v.data, Map())
        }
      }
      res.foreach { r =>
        outputs = outputs :+ r
      }
      if (els.nonEmpty) {
        addCell(Cell.Code(
          executionCount = executionCount,
          source = els.map(_.code).mkString("\n"),
          metadata = Cell.CodeMetadata(
            collapsed = false, autoscroll = false),
          outputs = outputs))
        this._executionCount += 1
      }
    }

    def flush(res: Option[Output]) = {
      flushCell(execLogs, Seq())
      currentExecLog.foreach { el =>
        val duration = System.currentTimeMillis - el.startAt
        val outs =
          if (duration > showTimeMillis)
            Seq(ipynb.Output.DisplayData(
              makeExecutionTime(duration).data, Map())) ++ res
          else
            res.toSeq
        flushCell(Seq(el), outs)
      }
      this.currentExecLog = None
      this.execLogs = Seq()
    }

    def build() = {
      flush(None)
      ipynb.Notebook(
        metadata = Map("language_info" -> json.JsObject(Map("name" -> json.JsString("scala")))),
        nbformat = 4,
        nbformatMinor = 0,
        cells)
    }

    override def finish() = flush(None)
  }

  class Log(writer: java.io.PrintWriter) extends Builder {
    private[this] val sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

    private[this] var startAt: Option[Long] = None
    private[this] var showTimeMillis = 5000L

    private[this] def newLine() = writer.println()

    private[this] def write(lines: Seq[String]): Unit = {
      val prefix = s"[${sdf.format(new java.util.Date())}] "
      lines.foreach { l =>
        writer.println(prefix + l)
      }
      writer.flush()
    }

    private[this] def write(content: String, prefix: String): Unit =
      write(
        content.split("\n").map(prefix + _))

    private[this] def flush() = {
      startAt.map(System.currentTimeMillis() - _)
        .filter(_ >= showTimeMillis)
        .foreach { duration =>
          write(makeExecutionTime(duration).text, "")
        }
      startAt = None
    }

    override def setShowTimeMillis(l: Long): Unit = {
      showTimeMillis = l
    }

    override def code(src: String): Unit = {
      flush()
      newLine()
      startAt = Some(System.currentTimeMillis())
      write(src, "> ")
    }

    override def markdown(src: String): Unit = {}

    override def quiet(): Unit = {}

    override def expr(value: Unit): Unit = {}
    override def expr(value: Nothing): Unit = {}

    override def expr[A: Format](value: A): Unit =
      expr(implicitly[Format[A]].apply(value))

    override def expr(value: Value): Unit = {
      val s = value.text
      write(s, "=> ")
      flush()
    }

    override def error(t: Throwable)(implicit format: ErrorFormat): Unit = {
      write(t.toString, "ERR: ")
      write(t.getStackTrace.map("ERR:   " + _.toString))
      flush()
    }

    override def stdout(s: String): Unit = write(s, "stdout: ")
    override def stderr(s: String): Unit = write(s, "stderr: ")
    override def display(v: Value): Unit = write(v.text, "display: ")

    override def finish() = flush()
  }

  class Multiplex(children: Seq[Builder]) extends Builder {
    private[this] def exec(f: Builder => Unit) =
      children.foreach(f)

    override def setShowTimeMillis(l: Long): Unit = exec(_.setShowTimeMillis(l))

    override def code(src: String): Unit = exec(_.code(src))

    override def markdown(src: String): Unit = exec(_.markdown(src))

    override def quiet(): Unit = exec(_.quiet())

    override def expr(value: Unit): Unit = exec(_.expr(value))
    override def expr[A: Format](value: A): Unit = exec(_.expr(value))
    override def expr(value: Value): Unit = exec(_.expr(value))

    override def error(t: Throwable)(implicit format: ErrorFormat): Unit = exec(_.error(t))

    override def stdout(s: String): Unit = exec(_.stdout(s))
    override def stderr(s: String): Unit = exec(_.stderr(s))
    override def display(v: Value): Unit = exec(_.display(v))

    override def finish() = exec(_.finish())
  }

}

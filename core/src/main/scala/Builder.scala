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

  def finish(): Unit = {}
}

object Builder {
  case class ExecLog(
    code: String,
    startAt: Long,
    stdout: Seq[String],
    stderr: Seq[String]) {
    def addStdout(s: String) = copy(stdout = this.stdout :+ s)
    def addStderr(s: String) = copy(stderr = this.stderr :+ s)
  }
  object ExecLog {
    def apply(code: String, startAt: Long): ExecLog = ExecLog(code, startAt, Seq(), Seq())
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
      this.currentExecLog = currentExecLog.map(_.addStdout(s))
    }

    override def stderr(s: String) = {
      this.currentExecLog = currentExecLog.map(_.addStderr(s))
    }

    private[this] def flushCell(els: Seq[ExecLog], res: Seq[Output]): Unit = {
      def nl(s: String) = if (s.nonEmpty && s.last != '\n') s + "\n" else s
      var outputs = Seq.empty[Output]
      els.foreach { el =>
        if (el.stdout.nonEmpty) {
          outputs = outputs :+ Output.Stream(
            "stdout",
            nl(el.stdout.mkString("")))
        }
        if (el.stderr.nonEmpty) {
          outputs = outputs :+ Output.Stream(
            "stderr",
            nl(el.stderr.mkString("")))
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

    private[this] def makeExecutionTime(millis: Long): ipynb.Output =
      ipynb.Output.DisplayData(Value.text(f"Execution time: ${millis / 1000.0}%.2f[Sec]").data, Map())

    def flush(res: Option[Output]) = {
      flushCell(execLogs, Seq())
      currentExecLog.foreach { el =>
        val duration = System.currentTimeMillis - el.startAt
        val outs =
          if (duration > showTimeMillis)
            Seq(makeExecutionTime(duration)) ++ res
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
          write(f"Execution time: ${duration / 1000.0}%.2f[Sec]", "")
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

    override def finish() = flush()
  }

  class Multiplex(children: Seq[Builder]) extends Builder {
    override def setShowTimeMillis(l: Long): Unit = children.foreach(_.setShowTimeMillis(l))

    override def code(src: String): Unit = children.foreach(_.code(src))

    override def markdown(src: String): Unit = children.foreach(_.markdown(src))

    override def quiet(): Unit = children.foreach(_.quiet())

    override def expr(value: Unit): Unit = children.foreach(_.expr(value))
    override def expr[A: Format](value: A): Unit = children.foreach(_.expr(value))
    override def expr(value: Value): Unit = children.foreach(_.expr(value))

    override def error(t: Throwable)(implicit format: ErrorFormat): Unit = children.foreach(_.error(t))

    override def stdout(s: String): Unit = children.foreach(_.stdout(s))
    override def stderr(s: String): Unit = children.foreach(_.stderr(s))
    override def finish() = children.foreach(_.finish())
  }

}

package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Cell
import com.todesking.scalanb.ipynb.Output

trait Builder {
  def setShowTimeMillis(l: Long): Unit

  protected def executionCount: Int

  protected def flush(res: Option[Output]): Unit

  def code(src: String): Unit

  def markdown(src: String): Unit

  def quiet(): Unit

  def expr(value: Unit): Unit = {}
  def expr(value: Nothing): Unit = {}

  def expr(value: Value): Unit =
    flush(Some(Output.ExecuteResult(value.data, Map(), executionCount)))

  def expr[A: Format](value: A): Unit =
    expr(implicitly[Format[A]].apply(value))

  def markdownCell(src: String): Unit

  def error(t: Throwable)(implicit format: ErrorFormat): Unit =
    flush(Some(format.apply(t)))

  def stdout(s: String): Unit
  def stderr(s: String): Unit

  def build(): ipynb.Notebook
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

  class OnMemory extends Builder {
    private[this] var _executionCount = 1
    override def executionCount = _executionCount

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

    override def markdownCell(s: String) = {
      flush(None)
      addCell(Cell.Markdown(s))
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

    override def flush(res: Option[Output]) = {
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

    override def build() = {
      flush(None)
      ipynb.Notebook(
        metadata = Map(),
        nbformat = 4,
        nbformatMinor = 0,
        cells)
    }
  }

}

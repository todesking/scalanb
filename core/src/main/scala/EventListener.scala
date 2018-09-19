package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Cell
import com.todesking.scalanb.ipynb.Output

import play.api.libs.json

trait EventListener {
  def event(state: NBState, e: Event): Unit
}

object EventListener {
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
    val text = format.Time.fromMillis(millis)
    Value.text(f"Execution time: $text")
  }

  class Ipynb extends EventListener {
    private[this] var _executionCount = 1
    def executionCount = _executionCount

    private[this] var cells = Seq.empty[Cell]

    private[this] var execLogs = Seq.empty[ExecLog]
    private[this] var currentExecLog: Option[ExecLog] = None

    private[this] def addCell(c: Cell) = {
      this.cells = this.cells :+ c
    }

    override def event(state: NBState, e: Event) = e match {
      case Event.WholeCode(src) =>
      case Event.Expr(value) =>
        flush(Some(Output.ExecuteResult(value.data, Map(), executionCount)), state)
      case Event.Error(t, format) =>
        flush(Some(format.apply(t)), state)
      case Event.Quiet() =>
        this.currentExecLog = None
      case Event.Markdown(src) =>
        flush(None, state)
        addCell(Cell.Markdown(src))
      case Event.Code(s) =>
        currentExecLog.foreach { el =>
          val duration = System.currentTimeMillis() - el.startAt
          if (duration > state.config.showTimeMillis) {
            // First, flush previous exec logs
            this.currentExecLog = None
            flush(None, state)
            // Then flush current one
            this.currentExecLog = Some(el)
            flush(None, state)
          } else {
            this.execLogs = this.execLogs :+ el
          }
        }
        this.currentExecLog = Some(ExecLog(s, System.currentTimeMillis()))
      case Event.StdOut(s) =>
        this.currentExecLog = currentExecLog.map(_.content(Content.StdOut(s)))
      case Event.StdErr(s) =>
        this.currentExecLog = currentExecLog.map(_.content(Content.StdErr(s)))
      case Event.Display(v) =>
        this.currentExecLog = currentExecLog.map(_.content(Content.Display(v)))
      case Event.Finish() =>
        flush(None, state)
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
          executionCount = Some(executionCount),
          source = els.map(_.code).mkString("\n"),
          metadata = Cell.CodeMetadata(
            collapsed = false, autoscroll = false),
          outputs = outputs))
        def extract(data: Map[String, json.JsValue]) =
          data.zipWithIndex.collect {
            case (("text/csv", json.JsString(s)), n) =>
              (s"out-$executionCount-$n.csv", "text/csv", s)
          }
        outputs.flatMap {
          case Output.ExecuteResult(data, metadata, count) =>
            extract(data)
          case Output.DisplayData(data, metadata) =>
            extract(data)
          case _ => Seq()
        }.foreach {
          case (name, mime, content) =>
            // TODO: html escape
            addCell(Cell.Markdown(
              s"Attachment: [$name](attachment:$name)",
              Map(name -> Map(mime -> json.JsString(java.util.Base64.getEncoder.encodeToString(content.getBytes))))))
        }
        this._executionCount += 1
      }
    }

    def flush(res: Option[Output], state: NBState) = {
      flushCell(execLogs, Seq())
      currentExecLog.foreach { el =>
        val duration = System.currentTimeMillis - el.startAt
        val outs =
          if (duration > state.config.showTimeMillis)
            Seq(ipynb.Output.DisplayData(
              makeExecutionTime(duration).data, Map())) ++ res
          else
            res.toSeq
        flushCell(Seq(el), outs)
      }
      this.currentExecLog = None
      this.execLogs = Seq()
    }

    def build(state: NBState) = {
      flush(None, state)
      ipynb.Notebook(
        metadata = Map("language_info" -> json.JsObject(Map("name" -> json.JsString("scala")))),
        nbformat = 4,
        nbformatMinor = 0,
        cells)
    }
  }

  class Log(writer: java.io.PrintWriter) extends EventListener {
    private[this] val sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

    private[this] var startAt: Option[Long] = None

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

    private[this] def flush(state: NBState) = {
      startAt.map(System.currentTimeMillis() - _)
        .filter(_ >= state.config.showTimeMillis)
        .foreach { duration =>
          write(makeExecutionTime(duration).text, "")
        }
      startAt = None
    }

    override def event(state: NBState, e: Event) = e match {
      case Event.WholeCode(s) =>
      case Event.Code(src) =>
        flush(state)
        newLine()
        startAt = Some(System.currentTimeMillis())
        write(src, "> ")
      case Event.Markdown(s) =>
      case Event.Expr(value) =>
        val s = value.text
        write(s, "=> ")
        flush(state)
      case Event.Error(t, format) =>
        write(t.toString, "ERR: ")
        write(t.getStackTrace.map("ERR:   " + _.toString))
        flush(state)
      case Event.StdOut(s) => write(s, "stdout: ")
      case Event.StdErr(s) => write(s, "stderr: ")
      case Event.Display(v) => write(v.text, "display: ")
      case Event.Quiet() =>
      case Event.Finish() => flush(state)
    }
  }
}

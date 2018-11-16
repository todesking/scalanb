package com.todesking.scalanb

import com.todesking.scalanb.io.TappedPrintStream
import com.todesking.scalanb.io.IO
import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.cache.Checkpoint
import com.todesking.scalanb.cache.DepID
import com.todesking.scalanb.cache.MetaData

object Runner {
  def run[A](ctx: NBContext, silent: Boolean)(f: NBContext => Unit): Unit = {
    val tappedOut = TappedPrintStream(System.out, silent) { str =>
      ctx.event.send(Event.StdOut(str))
    }
    val tappedErr = TappedPrintStream(System.out, silent) { str =>
      ctx.event.send(Event.StdErr(str))
    }

    IO.withOuts(tappedOut, tappedErr) {
      try {
        f(ctx)
      } catch {
        case e: Exit =>
        case e: Throwable =>
          ctx.event.send(Event.Error(e, ctx.state.config.errorFormat))
          // TODO: Write incomplete notebook
          throw e
      } finally {
        ctx.event.send(Event.Finish())
      }
    }
  }

  def newLogName(name: String): String = {
    val sdf = new java.text.SimpleDateFormat("yyyyMMdd_HHmmss")
    s"${sdf.format(new java.util.Date())}_$name"
  }

  case class Args(out: Out, useLog: Boolean, ipynbOnError: Boolean, saveSource: Boolean, fsForCache: FileSystem)

  def defaultHistPathFor(fsName: String): String =
    s"${FileSystem.getFactory(fsName).homePath}/.scalanb/hist"

  def defaultCachePathFor(fsName: String): String =
    s"${FileSystem.getFactory(fsName).homePath}/.scalanb/cache"

  def parseArgs(args: Seq[String]): (Args, Seq[String]) = {
    val rest = {
      val a = args.dropWhile(_ != "--")
      if (a.isEmpty) a else a.tail
    }
    val opts = args.takeWhile(_ != "--")

    var fsForCache = FileSystem.newFS("file", defaultCachePathFor("file"))
    var outs = Seq.empty[Out]
    var useLog = false
    var ipynbOnError = true
    var saveSource = false

    val id = "[a-zA-Z0-9_.]+"
    val outOptionPattern = s"--out=($id)(?::(.+))?".r
    val cacheOptionPattern = s"--cache=($id)(?::(.+))?".r
    val ipynbOnErrorPattern = "--ipynb-on-error=(true|false)".r
    val saveSourcePattern = "--save-source=(true|false)".r

    def parseKV(arg: String) =
      if (arg == null) Map.empty[String, String]
      else arg.split(",").map { kv => kv.split("=") match { case Array(k, v) => (k, v) } }.toMap

    opts.foreach {
      case `outOptionPattern`(fsName, subArgs) =>
        val kvs = parseKV(subArgs)
        val path = kvs.get("path") getOrElse defaultHistPathFor(fsName)
        outs = outs :+ new FSOut(FileSystem.newFS(fsName, path))
      case `cacheOptionPattern`(fsName, subArgs) =>
        val kvs = parseKV(subArgs)
        val path = kvs.get("path") getOrElse defaultCachePathFor(fsName)
        fsForCache = FileSystem.newFS(fsName, path)
      case "--log" =>
        useLog = true
      case `ipynbOnErrorPattern`(b) =>
        ipynbOnError = b match {
          case "true" => true
          case "false" => false
        }
      case `saveSourcePattern`(b) =>
        saveSource = b match {
          case "true" => true
          case "false" => false
        }
    }
    val theOut = outs match {
      case Seq() => new FSOut(FileSystem.newFS("file", defaultHistPathFor("file")))
      case Seq(o) => o
      case xs => new MultiOut(xs)
    }
    (Args(theOut, useLog, ipynbOnError, saveSource, fsForCache), rest)
  }

  def runBatch(args: Array[String], notebookName: String, notebookClassName: String, src: String)(invoke: NBContext => Unit): Unit = {
    val start = System.currentTimeMillis()

    val (parsedArgs, _) = parseArgs(args)

    val logName = newLogName(notebookName)
    val out = parsedArgs.out

    val logWriter =
      if (parsedArgs.useLog) Some(new java.io.PrintWriter(out.openLog(logName)))
      else None

    val ipynbListener = new EventListener.Ipynb()
    val listeners = logWriter.fold[Seq[EventListener]](Seq(ipynbListener)) { w =>
      Seq(ipynbListener, new EventListener.Log(w))
    }

    val newCP = { ctx: NBContext => new Checkpoint(parsedArgs.fsForCache, new CacheLogger(ctx)) }

    val ctx = new NBContext(notebookName, notebookClassName, listeners, newCP)

    def writeIpynb() = {
      val duration = System.currentTimeMillis() - start
      ctx.event.send(Event.Markdown(s"```\nTotal execution time: ${format.Time.fromMillis(duration)}\n```"))
      out.notebook(logName, ipynbListener.build(ctx.state))
    }

    if (parsedArgs.saveSource)
      out.write(s"$logName.scala", src)

    try {
      run(ctx, silent = false)(invoke)
    } catch {
      case e: Throwable =>
        if (parsedArgs.ipynbOnError) writeIpynb()
        throw e
    } finally {
      logWriter.foreach(_.close())
    }
    writeIpynb()
  }

  class CacheLogger(ctx: NBContext) extends cache.CacheEventListener {
    import com.todesking.scalanb.format.Html.h

    private[this] def time(millis: Long): String =
      format.Time.fromMillis(millis)

    private[this] def display(v: Value) = ctx.event.send(Event.Display(v))
    private[this] def localDateTimeString(i: java.time.Instant) = {
      val ldt = java.time.OffsetDateTime.ofInstant(i, java.time.ZoneId.systemDefault())
      val format = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
      format.format(ldt)
    }
    private[this] def showMeta(meta: MetaData): String = s"""
    |<details>
    |<summary>${h(meta.id.shortString)}</summary>
    |<dl>
    |<dt>ID</dt><dd>${showId(meta.id)}</dd>
    |<dt>Created at<dt><dd>${h(localDateTimeString(meta.createdAt))}</dd>
    |<dt>Calc duration</dt><dd>${h(time(meta.calcDuration))}</dd>
    |<dt>Save duration</dt><dd>${h(time(meta.saveDuration))}</dd>
    |</dl>
    |</details>""".stripMargin.drop(1)
    private[this] def showId(id: DepID): String = {
      val code = id match {
        case DepID.Root(ns, name, src, deps) => src
        case DepID.Map(parent, src) => src
        case _ => ""
      }
      val codeHtml =
        if (code == "") ""
        else
          s"""
          |<dt>Code</dt>
          |<dd><div class="highlight hl-scala">
          |  <pre>${h(code)}</pre>
          |</div></dd>""".stripMargin.drop(1)
      s"""
      |<details>
      |<summary>${h(id.shortString)}</summary>
      |<dl>
      |<dt>Namespace</dt><dd>${h(id.namespace)}</dd>
      |<dt>Name</dt><dd>${h(id.name)}</dd>
      |${codeHtml}
      |<dt>Deps</dt>
      |<dd><ul>${id.deps.map(showId).map(s => s"<li>${s}</li>").mkString("\n")}</ul></dd>
      |</dl>
      |</details>""".stripMargin.drop(1)
    }

    override def hit(fs: FileSystem, id: DepID, meta: MetaData) = {
      display(Value.text(s"${id.shortString}, created=${localDateTimeString(meta.createdAt)}, calc = ${time(meta.calcDuration)}, save = ${meta.saveDuration}"))
    }

    override def loaded(fs: FileSystem, id: DepID, meta: MetaData, loadDuration: Long) = {
      display(Value.text(f"Cache loaded: ${id.shortString}, duration = ${time(loadDuration)}"))
    }

    override def miss(fs: FileSystem, id: DepID) = {
      display(Value.text(s"Uncached: ${id.shortString}") ++ Value.html(s"""
        |Uncached: ${h(id.shortString)}
        |${showId(id)}""".stripMargin.drop(1)))
    }

    override def saved(fs: FileSystem, id: DepID, meta: MetaData) = {
      display(Value.text(f"Cache saved: ${id.shortString}, calc = ${time(meta.calcDuration)}, save = ${time(meta.saveDuration)}"))
    }
  }

}

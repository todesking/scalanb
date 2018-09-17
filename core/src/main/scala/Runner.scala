package com.todesking.scalanb

import com.todesking.scalanb.io.TappedPrintStream
import com.todesking.scalanb.io.IO
import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.cache.CacheFS

object Runner {
  def run[A](ctx: NBContext)(f: NBContext => A): A = {
    val tappedOut = TappedPrintStream(System.out) { str =>
      ctx.event.stdout(str)
    }
    val tappedErr = TappedPrintStream(System.out) { str =>
      ctx.event.stderr(str)
    }

    IO.withOuts(tappedOut, tappedErr) {
      try {
        f(ctx)
      } catch {
        case e: Throwable =>
          ctx.event.error(e)
          // TODO: Write incomplete notebook
          throw e
      }
    }
  }

  def newLogName(name: String): String = {
    val sdf = new java.text.SimpleDateFormat("yyyyMMdd_HHmmss")
    s"${sdf.format(new java.util.Date())}_$name"
  }

  case class Args(out: Out, useLog: Boolean, ipynbOnError: Boolean, saveSource: Boolean, fsForCache: FileSystem)

  def defaultHistPathFor(fsName: String): String =
    s"${FileSystem.getFactory("file").homePath}/.scalanb/hist"

  def defaultCachePathFor(fsName: String): String =
    s"${FileSystem.getFactory("file").homePath}/.scalanb/cache"

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

  def runBatch(args: Array[String], notebookName: String, src: String)(invoke: NBContext => Unit): Unit = {
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
    val cacheFS = new CacheFS(parsedArgs.fsForCache, notebookName)
    val ctx = new NBContext(notebookName, listeners, cacheFS)

    def writeIpynb() = {
      val duration = System.currentTimeMillis() - start
      ctx.event.markdown(s"```\nTotal execution time: ${format.Time.fromMillis(duration)}\n```")
      out.notebook(logName, ipynbListener.build())
    }

    if (parsedArgs.saveSource)
      out.write(s"$logName.scala", src)

    try {
      run(ctx)(invoke)
    } catch {
      case e: Throwable =>
        if (parsedArgs.ipynbOnError) writeIpynb()
        throw e
    } finally {
      logWriter.foreach(_.close())
    }
    writeIpynb()
  }
}

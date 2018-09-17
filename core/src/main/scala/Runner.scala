package com.todesking.scalanb

import com.todesking.scalanb.io.TappedPrintStream
import com.todesking.scalanb.io.IO
import com.todesking.scalanb.io.FileSystem

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

  case class Args(out: Out, useLog: Boolean, ipynbOnError: Boolean, saveSource: Boolean)

  def parseArgs(args: Seq[String]): (Args, Seq[String]) = {
    val rest = {
      val a = args.dropWhile(_ != "--")
      if (a.isEmpty) a else a.tail
    }
    val opts = args.takeWhile(_ != "--")

    var outs = Seq.empty[Out]
    var useLog = false
    var ipynbOnError = true
    var saveSource = false
    val id = "[a-zA-Z0-9_.]+"
    val outOptionPattern = s"--out=($id)(?::(.+))?".r
    val ipynbOnErrorPattern = "--ipynb-on-error=(true|false)".r
    val saveSourcePattern = "--save-source=(true|false)".r
    opts.foreach {
      case `outOptionPattern`(outType, outArgs) =>
        val parsedOutArgs =
          if (outArgs == null) Map.empty[String, String]
          else outArgs.split(",").map { kv => kv.split("=") match { case Array(k, v) => (k, v) } }.toMap
        outs = outs :+ new FSOut(FileSystem.newFS(outType, parsedOutArgs))
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
      case Seq() => new FSOut(FileSystem.newFS("file", Map()))
      case Seq(o) => o
      case xs => new MultiOut(xs)
    }
    (Args(theOut, useLog, ipynbOnError, saveSource), rest)
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
    val ctx = new NBContext(notebookName, listeners)

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

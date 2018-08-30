package com.todesking.scalanb

import com.todesking.scalanb.io.TappedPrintStream
import com.todesking.scalanb.io.IO

object Runner {
  def run[A](builder: Builder)(f: Builder => A): A = {
    val tappedOut = TappedPrintStream(System.out) { str =>
      builder.stdout(str)
    }
    val tappedErr = TappedPrintStream(System.out) { str =>
      builder.stderr(str)
    }

    IO.withOuts(tappedOut, tappedErr) {
      try {
        f(builder)
      } catch {
        case e: Throwable =>
          builder.error(e)
          // TODO: Write incomplete notebook
          throw e
      }
    }
  }

  def newLogName(name: String): String = {
    val sdf = new java.text.SimpleDateFormat("yyyyMMdd_HHmmss")
    s"${sdf.format(new java.util.Date())}_$name"
  }

  def newOut(outType: String, args: Map[String, String]): Out = {
    import scala.collection.JavaConverters._
    val loader = java.util.ServiceLoader.load(classOf[OutFactory])
    loader.iterator.asScala
      .toSeq
      .filter(_.name == outType)
      .headOption
      .getOrElse { throw new RuntimeException(s"Unknown out type: $outType") }
      .newOut(args)
  }

  type TargetType = {
    def scalanb__run(implicit builder: Builder): Unit
  }

  case class Args(out: Out, useLog: Boolean, ipynbOnError: Boolean)

  def parseArgs(args: Seq[String]): (Args, Seq[String]) = {
    val rest = {
      val a = args.dropWhile(_ != "--")
      if (a.isEmpty) a else a.tail
    }
    val opts = args.takeWhile(_ != "--")

    var outs = Seq.empty[Out]
    var useLog = false
    var ipynbOnError = true
    val id = "[a-zA-Z0-9_.]+"
    val outOptionPattern = s"--out=($id)(?::(.+))?".r
    val ipynbOnErrorPattern = "--ipynb-on-error=(true|false)".r
    opts.foreach {
      case `outOptionPattern`(outType, outArgs) =>
        val parsedOutArgs =
          if (outArgs == null) Map.empty[String, String]
          else outArgs.split(",").map { kv => kv.split("=") match { case Array(k, v) => (k, v) } }.toMap
        outs = outs :+ newOut(outType, parsedOutArgs)
      case "--log" =>
        useLog = true
      case `ipynbOnErrorPattern`(b) =>
        ipynbOnError = b match {
          case "true" => true
          case "false" => false
        }
    }
    val theOut = outs match {
      case Seq() => newOut("file", Map())
      case Seq(o) => o
      case xs => new MultiOut(xs)
    }
    (Args(theOut, useLog, ipynbOnError), rest)
  }

  def runBatch(args: Array[String], notebookName: String, target: TargetType): Unit = {
    import scala.language.reflectiveCalls
    runBatch(args, notebookName) { builder =>
      try {
        target.scalanb__run(builder)
      } catch {
        case e: java.lang.reflect.InvocationTargetException =>
          throw e.getCause
      }
    }
  }

  def runBatch(args: Array[String], notebookName: String)(invoke: Builder => Unit): Unit = {
    val start = System.currentTimeMillis()

    val (parsedArgs, _) = parseArgs(args)

    val logName = newLogName(notebookName)
    val out = parsedArgs.out

    val logWriter =
      if (parsedArgs.useLog) Some(new java.io.PrintWriter(out.openLog(logName)))
      else None

    val ipynbBuilder = new Builder.Ipynb()
    val builder = logWriter.fold[Builder](ipynbBuilder) { w =>
      new Builder.Multiplex(Seq(ipynbBuilder, new Builder.Log(w)))
    }

    def writeIpynb() = {
      val duration = System.currentTimeMillis() - start
      builder.markdown(s"Total execution time: ${format.Time.fromMillis(duration)}")
      val filePath = out.notebook(logName, ipynbBuilder.build())
      println(s"scalanb: Notebook log saved to ${filePath}")
    }

    try {
      run(builder)(invoke)
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

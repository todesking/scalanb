package test

import com.todesking.{ scalanb => nb }

import com.todesking.scalanb.{ Event => E }

class NotebookTest extends org.scalatest.FunSpec {
  def run(f: nb.NBContext => Unit): Seq[nb.Event] = {
    val el = new RecordEventListener
    val ctx = new nb.NBContext("Notebook", "class_of_note", Seq(el), null)
    try { nb.Runner.run(ctx, silent = true)(f) }
    catch { case e: RuntimeException => }
    el.events.map(_._2)
  }
  it("should record execution log: Empty") {
    val events = run { c => new NotebookTest.Empty()(c) }
    assert(events == Seq(E.Finish()))
  }
  it("should record execution log: Various") {
    val events = run { c => new NotebookTest.IO()(c) }
    assert(events == Seq(
      E.Code("""System.out.println("out via System")"""),
      E.StdOut("out via System\n"),
      E.Code("""Console.out.println("out via Console")"""),
      E.StdOut("out via Console\n"),
      E.Code("""System.err.println("err via System")"""),
      E.StdErr("err via System\n"),
      E.Code("""Console.err.println("err via Console")"""),
      E.StdErr("err via Console\n"),
      E.Finish()))
  }
  it("should record execution log: Error") {
    val events = run { c => new NotebookTest.Error()(c) }
    assert(events.size == 5)
    assert(events.take(3) == Seq(
      E.Code("123"),
      E.Expr(nb.Value.text("123")),
      E.Code("throw new RuntimeException()")))
    events(3) match {
      case E.Error(_, _) => // success
      case _ => assert(false)
    }
    events(4) == E.Finish()
  }
  it("should record execution log: Exit") {
    val events = run { c => new NotebookTest.Exit()(c) }
    assert(events == Seq(
      E.Code("1"),
      E.Expr(nb.Value.text("1")),
      E.Code("nb.exit()"),
      E.Finish()))
  }
}

object NotebookTest {
  implicit def textFormat[A]: nb.Format[A] = nb.Format[A] { a => nb.Value.text(a.toString) }
  @nb.Notebook
  class Empty {
  }
  @nb.Notebook
  class IO {
    System.out.println("out via System")
    Console.out.println("out via Console")
    System.err.println("err via System")
    Console.err.println("err via Console")
  }
  @nb.Notebook
  class Error {
    123
    throw new RuntimeException()
  }
  @nb.Notebook
  class Exit {
    1
    nb.exit()
  }
}


package test

import com.todesking.{ scalanb => nb }

class RecordEventListener extends nb.EventListener {
  var events = Seq.empty[(nb.NBState, nb.Event)]
  override def event(st: nb.NBState, e: nb.Event) =
    events = events :+ (st -> e)
}

class SrcTest extends org.scalatest.FunSpec {
  it("Notebook should have its source") {
    assert(SrcTest.NotebookTest.scalanb__source == """"hello"
      |val m = NotebookTest.DeepModule.load(1)
      |val m2 = ShallowModule.load()
      |m.a""".stripMargin)
  }
  it("Notebook should record its execution") {
    val el = new RecordEventListener
    val ctx = new nb.NBContext("Notebook", classOf[SrcTest.NotebookTest].getName, Seq(el), null)
    new SrcTest.NotebookTest()(ctx)
    import nb.{ Event => E }
    assert(el.events.map(_._2) == Seq(
      E.Code(""""hello""""),
      E.Expr(nb.Format.of[String].apply("hello")),
      E.Code("val m = NotebookTest.DeepModule.load(1)"),
      E.EnterModule(),
      E.Code("val a = x + 2"),
      E.Code("val b = a"),
      E.ExitModule("DeepModule", classOf[SrcTest.NotebookTest.DeepModule].getName),
      E.Code("val m2 = ShallowModule.load()"),
      E.EnterModule(),
      E.Code("val x = 1"),
      E.ExitModule("ShallowModule", classOf[SrcTest.ShallowModule].getName),
      E.Code("m.a"),
      E.Expr(nb.Format.of[Int].apply(3))))
  }
}

object SrcTest {
  @nb.Notebook
  class NotebookTest {
    "hello"

    val m = NotebookTest.DeepModule.load(1)
    val m2 = ShallowModule.load()

    m.a
  }

  @nb.Module
  class ShallowModule {
    val x = 1
  }
  object NotebookTest {
    @nb.Module
    class DeepModule(x: Int) {
      val a = x + 2
      val b = a
    }
  }
}

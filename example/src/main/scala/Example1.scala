import com.todesking.{ scalanb => nb }

@nb.Module
class Module(param1: Int) {
  val moduleValue = param1 * 100
  val m2 = Module2.load(moduleValue)
  nb.checkpoint.fs.className
}

@nb.Module
class Module2(param: Int) {
  val x = param + 1
  nb.checkpoint.fs.className
}

@nb.Notebook
class Example1 {
  nb.setShowTimeMillis(100)
  val cp = nb.checkpoint
  cp.fs.className

  nb.markdown("# Scalanb Example")

  val (a, b) = (1, 2)
  a

  val aa, bb, cc = 1; aa

  val myModule = Module.load(param1 = 10)
  import myModule._

  moduleValue

  println(s"a = $a")
  println("hello")

  nb.markdown("This is $a + b$:")
  a + b

  System.out.println("Hello from System.out.println")
  System.err.println("Boo!")
  println("Hello from Console.println")
  Console.err.println("Boo!2")
  (0 until 2).foreach { i =>
    nb.display(i)
  }

  def f(xxx: Int) = {
    xxx * xxx * xxx
  }

  f(10); f(99)

  val x = 2
  val y = 100
  Thread.sleep(x * y)

  locally {
    Thread.sleep(300)
    100
  }

  Example1.foo()

  val cachedValue = cp.cache((a, b)) {
    case (a, b) =>
      println("Calculating...")
      a + b
  }
  cachedValue

  val colNames =
    Seq("id", "name", "size")

  val rows =
    Seq(
      "1,Int,4",
      "2,Object,8",
      "3,Long,8",
      "4,Short,2").map(_.split(",").toSeq)

  nb.table(colNames, rows)
  nb.vtable(colNames, rows)

  def repeat(x: Int) = (0 until x).foreach { i =>
    nb.inspect {
      i
      i + 1
    }
  }

  repeat(3)

  throw new RuntimeException("Nested exception", new RuntimeException("Inner exception"))
}

object Example1 {
  def foo() = println(1)
}

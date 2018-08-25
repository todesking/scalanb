import com.todesking.{ scalanb => nb }

@nb.Notebook
class Example1 {
  nb.setShowTimeMillis(100)

  nb.markdown("# Scalanb Example")

  val a = 1
  val b = 2
  a

  println(s"a = $a")
  println("hello")

  nb.markdown("This is $a + b$:")
  a + b

  System.out.println("Hello from System.out.println")
  println("Hello from Console.println")
  System.err.println("Boo!")
  Console.err.println("Boo!2")

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

  ???
}

import com.todesking.{ scalanb => nb }

@nb.Notebook
class Example1 {
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
    xxx * xxx
  }

  f(10); f(20)

  val x = 6
  val y = 1000
  Thread.sleep(x * y)

  ???
}

package test

import com.todesking.scalanb.cache.{ DepID, Dep, Cacheable, Decomposable, Dependable, MetaData }
import com.todesking.scalanb.io.{ FileSystem, LocalFileSystem }
import scala.reflect.runtime.universe.TypeTag

import com.todesking.{ scalanb => nb }

class CacheTest extends org.scalatest.FunSpec {
  import test.io.FileSystemTestUtil.withTmpDir

  val cache = nb.cache.get(getClass)

  def withFS(f: FileSystem => Unit): Unit =
    withTmpDir { tmp => f(new LocalFileSystem(tmp.toString)) }
  def withCache(f: nb.cache.CacheContext => Unit) = withFS { fs =>
    f(new nb.cache.CacheContext(fs))
  }

  def assertCacheable[A: Cacheable: TypeTag](a: A): Unit = withCache { implicit ctx =>
    def eq(a: A): Any = a match {
      case x: Array[_] => x.toSeq
      case x => x
    }
    def exec() = {
      val x = cache.cache0 { a }
      cache.unwrap(x) { x => assert(eq(x) == eq(a)) }
    }
    exec()
    exec()
  }

  describe("Caching") {
    it("should save/restore data")(withCache { implicit ctx =>
      var count = 0
      def exec() = {
        val x = cache.source { 1 }
        val y = 100
        val z = cache.cache0 { 1 }
        val w = cache.cache((x, y)) {
          case (x, y) =>
            count += 1
            x + y
        }
        assert(!ctx.fs.exists(x.id.pathString))
        assert(ctx.fs.exists(z.id.pathString))
        assert(ctx.fs.exists(w.id.pathString))

        w.unwrapUNSAFE
      }
      assert(count == 0)
      assert(exec() == 101)
      assert(count == 1)
      assert(exec() == 101)
      assert(count == 1)
    })
    it("should save metadata")(withCache { implicit ctx =>
      val a = cache.cache0 { 1 }
      val (b, c) = a.map { a => (a, a + 1) }.decompose
      val d = cache.cache((b, c)) { case (b, c) => b + c }
      d.foreach { d => assert(d == 3) }

      val nfs = ctx.fs.namespace(d.id.pathString)
      assert(nfs.exists("cache.json"))
      val meta = MetaData.fromJson(nfs.readString("cache.json"))
      assert(meta.id == d.id)
    })
    it("should distinct values via its tree")(withCache { implicit ctx =>
      val x = cache.source { 1 }
      val x1 = x.map(_.toString)
      val x2 = x.map(_.toString)
      assert(x1.id == x2.id)

      val y = cache.source((1, 2))
      val y1 = y.map { case (a, b) => a + b }
      val y2 = y.map { case (a, b) => a + b }
      assert(y1.id == y2.id)
    })
    it("should record its dependency")(withCache { implicit ctx =>
      val x = cache.source { 1 }
      val y = cache.join(x) { x => x + 1 }
      assert(y.id.deps == Seq(x.id))
    })
    it("should accept anon cache")(withCache { implicit ctx =>
      def foo(a: Dep[Int]) = {
        println(a.id)
        assert(a.id.name == "__anon__")
      }
      foo(cache.cache0 { 1 })
      class A {
        foo(cache.cache0 { 1 })
      }
      new A
    })
  }
  describe("Decomposable") {
    it("should decompose dep value")(withCache { implicit ctx =>
      var count = 0
      def exec(): Dep[(Int, Int, Int)] = {
        val (a, b) = cache.source { (1, 2) }.decompose

        cache.unwrap((a, b)) {
          case (a, b) =>
            assert(a == 1)
            assert(b == 2)
        }

        cache.cache((a, b)) {
          case (a, b) =>
            count += 1
            (a + 1, b + 1, a + b)
        }
      }

      assert(count == 0)
      cache.unwrap(exec()) { x => assert(x == ((2, 3, 3))) }
      assert(count == 1)
      cache.unwrap(exec()) { x => assert(x == ((2, 3, 3))) }
      assert(count == 1)
    })

    def decompose[A, B](v: A)(implicit ev: Decomposable[A, B]): (DepID, B) = {
      val d = cache.source(v)
      (d.id, d.decompose)
    }
    it("should decompose tuple2")({
      val (id, (a, b)) = decompose((1, 2))
      assert((a.id, b.id) == ((id.item("_1"), id.item("_2"))))
      assert((a.unwrapUNSAFE, b.unwrapUNSAFE) == ((1, 2)))
    })
    it("should decompose Seq")({
      val (id, s) = decompose(Seq(1, 2, 3))
      assert(s.map(_.id) == Seq(id.item("0"), id.item("1"), id.item("2")))
      assert(s.map(_.unwrapUNSAFE) == Seq(1, 2, 3))
    })
  }
  describe("Cacheable") {
    it("should cache Int") {
      assertCacheable(1)
      assertCacheable(0)
      assertCacheable(-1)
    }
    it("should cache String") {
      assertCacheable("")
      assertCacheable("abc")
      assertCacheable("ABC123")
    }
    it("should cache Array[Int]") {
      assertCacheable(Array[Int]())
      assertCacheable(Array[Int](1, 2, 3))
    }
    it("should cache Seq[Int]") {
      assertCacheable(Seq[Int]())
      assertCacheable(Seq[Int](1, 2, 3))
    }
    it("should cache Seq[Custom]") {
      import CacheTest.Custom
      implicit val c = Cacheable.ofSerializable[Custom]
      assertCacheable(Custom(1))
      assertCacheable(Seq(Custom(1), Custom(2)))
    }
  }
  describe("Dep") {
    it("should provide map and foreach")(withCache { implicit ctx =>
      val x = cache.source { 10 }
      val y = x.map(_ + 11)
      y.foreach { y =>
        assert(y == 21)
      }
      val z = y.map(_ + 12).map(_ * 10)
      z.foreach { z =>
        assert(z == 330)
      }
      assert(y.map(_ + 1).id == y.map(_ + 1).id)
    })
  }
  describe("Dependable") {
    it("Int is dependable") {
      val d = implicitly[Dependable[Int, Int]]
      assert(d(1).id == DepID.forValue("int.1", "1", Seq()))
    }
    it("Option[A] is dependable") {
      val d = implicitly[Dependable[Option[Int], Option[Int]]]
      val id1 = implicitly[Dependable[Int, Int]].apply(1).id
      assert(d(Some(1)).id == DepID.forValue("option.Some(int.1)", "Some(int.1)", Seq(id1)))
      assert(d(None).id == DepID.forValue("option.None", "None", Seq()))
    }
  }
}

object CacheTest {
  case class Custom(i: Int) extends java.io.Serializable
}

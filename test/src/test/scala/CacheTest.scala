package test

import com.todesking.scalanb.cache.{ DepID, Checkpoint, Dep, Cacheable, Decomposable, Dependable }
import com.todesking.scalanb.io.{ FileSystem, LocalFileSystem }

class CacheTest extends org.scalatest.FunSpec {
  import test.io.FileSystemTestUtil.withTmpDir

  def withFS(f: FileSystem => Unit): Unit =
    withTmpDir { tmp => f(new LocalFileSystem(tmp.toString)) }

  def assertCacheable[A: Cacheable](a: A): Unit = withFS { fs =>
    val cp = new Checkpoint(fs)
    def eq(a: A): Any = a match {
      case x: Array[_] => x.toSeq
      case x => x
    }
    def exec() = {
      val x = cp.cache0 { a }
      cp.unwrap(x) { x => assert(eq(x) == eq(a)) }
    }
    exec()
    exec()
  }

  describe("Caching") {
    it("should save/restore data")(withFS { fs =>
      val cp = new Checkpoint(fs)
      var count = 0
      def exec() = {
        val x = cp.source { 1 }
        val y = 100
        val z = cp.cache0 { 1 }
        val w = cp.cache((x, y)) {
          case (x, y) =>
            count += 1
            x + y
        }
        assert(!fs.exists(Checkpoint.pathString(x.id)))
        assert(fs.exists(Checkpoint.pathString(z.id)))
        assert(fs.exists(Checkpoint.pathString(w.id)))

        w.unwrapUNSAFE
      }
      assert(count == 0)
      assert(exec() == 101)
      assert(count == 1)
      assert(exec() == 101)
      assert(count == 1)
    })
  }
  describe("Decomposable") {
    it("should decompose dep value")(withFS { fs =>
      val cp = new Checkpoint(fs)
      var count = 0
      def exec(): Dep[(Int, Int, Int)] = {
        val (a, b) = cp.source { (1, 2) }.decompose

        cp.unwrap((a, b)) {
          case (a, b) =>
            assert(a == 1)
            assert(b == 2)
        }

        cp.cache((a, b)) {
          case (a, b) =>
            count += 1
            (a + 1, b + 1, a + b)
        }
      }

      assert(count == 0)
      cp.unwrap(exec()) { x => assert(x == ((2, 3, 3))) }
      assert(count == 1)
      cp.unwrap(exec()) { x => assert(x == ((2, 3, 3))) }
      assert(count == 1)
    })

    def decompose[A, B](v: A)(implicit ev: Decomposable[A, B]): (DepID, B) = {
      val cp = new Checkpoint(null)
      val d = cp.source(v)
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
    it("should provide map and foreach")(withFS { fs =>
      val cp = new Checkpoint(fs)
      val x = cp.source { 10 }
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

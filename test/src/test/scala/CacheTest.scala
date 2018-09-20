package test

import com.todesking.scalanb.cache.{ CacheFS, DepID, Checkpoint, Dep }

class CacheTest extends org.scalatest.FunSpec {
  class MemoryFS extends CacheFS(null, "test") {
    var cache = scala.collection.mutable.Map.empty[DepID, Array[Byte]]
    override def path(id: DepID) = ???
    override def write(id: DepID, data: Array[Byte]) = {
      cache(id) = data
    }
    override def read(id: DepID) = {
      cache.get(id)
    }
  }

  class Fixture {
    val fs = new MemoryFS
    val cp = new Checkpoint(fs)
  }

  describe("Caching Int value") {
    it("should save/restore data")(new Fixture {
      var count = 0
      def exec() = {
        val x = cp.nocache { 1 }
        val y = 100
        val z = cp.cache0 { 1 }
        val w = cp.cache((x, y)) {
          case (x, y) =>
            count += 1
            x + y
        }
        assert(!fs.cache.contains(x.id))
        assert(fs.cache.contains(z.id))
        assert(fs.cache.contains(w.id))

        w.unwrapUNSAFE
      }
      assert(count == 0)
      assert(exec() == 101)
      assert(count == 1)
      assert(exec() == 101)
      assert(count == 1)
    })
  }
  describe("Caching Tuple") {
    it("should cache and decompose")(new Fixture {
      var count = 0
      def exec(): Dep[(Int, Int, Int)] = {
        val (a, b) = cp.nocache { (1, 2) }.decompose

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
  }
}

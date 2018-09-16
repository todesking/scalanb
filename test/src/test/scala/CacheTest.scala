package test

import com.todesking.scalanb.cache.{ IO, DepID, Checkpoint }

class CacheTest extends org.scalatest.FunSpec {
  class MemoryIO extends IO {
    var cache = scala.collection.mutable.Map.empty[DepID, Array[Byte]]
    override def protocol = ???
    override def path(id: DepID) = ???
    override def write(id: DepID, data: Array[Byte]) = {
      cache(id) = data
    }
    override def read(id: DepID) =
      cache.get(id)
  }

  class Fixture {
    val io = new MemoryIO
    val cp = new Checkpoint(io)
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
        w.unwrapUNSAFE
      }
      assert(count == 0)
      assert(exec() == 101)
      assert(count == 1)
      assert(exec() == 101)
      assert(count == 1)
    })
  }
}

package test.spark.cache

import org.apache.spark.sql.SparkSession

import com.todesking.scalanb.io.LocalFileSystem
import com.todesking.scalanb.cache.Checkpoint
import com.todesking.scalanb.cache.CacheFS

import test.io.FileSystemTestUtil

class SparkCacheTest extends org.scalatest.FunSpec {
  import FileSystemTestUtil.withTmpDir

  def withSpark(f: SparkSession => Unit): Unit = {
    val spark = SparkSession.builder()
      .master("local")
      .config("spark.sql.orc.impl", "native")
      .getOrCreate()
    try {
      f(spark)
    } finally {
      spark.stop()
    }
  }

  it("should cache DataFrame")(withTmpDir { tmp =>
    withSpark { implicit spark =>
      val fs = new LocalFileSystem(tmp.toString)
      val cfs = new CacheFS(fs, "test")
      val cp = new Checkpoint(cfs)

      import spark.implicits._
      import com.todesking.scalanb.spark.AllImplicits._

      var count = 0
      def exec() = {
        val df = cp.source {
          spark.createDataset(Seq(1, 2, 3)).toDF("v")
        }

        val df2 = cp.cache(df) { df =>
          count += 1
          df.select(('v + 1).as("v"))
        }

        val Array(train, test) = cp.cache(df) { df =>
          df.randomSplit(Array(1.0, 0.0))
        }.decompose

        cp.unwrap((train, test)) {
          case (train, test) =>
            assert(train.count() == 3)
            assert(test.count() == 0)
        }

        df2
      }

      assert(count == 0)
      cp.unwrap(exec()) { df =>
        assert(df.as[Int].collect().toSeq == Seq(2, 3, 4))
      }
      assert(count == 1)
      cp.unwrap(exec()) { df =>
        assert(df.as[Int].collect().toSeq == Seq(2, 3, 4))
      }
      assert(count == 1)
    }
  })

}

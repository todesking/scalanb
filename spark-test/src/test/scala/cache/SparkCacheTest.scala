package test.spark.cache

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.types
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.ml.linalg.Vector
import org.apache.spark.sql.{ functions => fn }

import com.todesking.scalanb.io.LocalFileSystem
import com.todesking.scalanb.cache.Checkpoint
import com.todesking.scalanb.cache.Cacheable

import test.io.FileSystemTestUtil

class SparkCacheTest extends org.scalatest.FunSpec {
  import FileSystemTestUtil.withTmpDir

  implicit val vectorEncoder = new org.apache.spark.sql.Encoder[Vector] {
    import types._
    override val schema = StructType(Seq(
      StructField("type", ByteType, nullable = false),
      StructField("size", IntegerType, nullable = true),
      StructField("indices", ArrayType(IntegerType, containsNull = false), nullable = true),
      StructField("values", ArrayType(DoubleType, containsNull = false), nullable = true)))
    override val clsTag = scala.reflect.classTag[Vector]
  }

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
      val cp = new Checkpoint(fs)

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

  it("shoud cache DataFrame with ml vectors")(withSpark { implicit spark =>
    withTmpDir { tmp =>
      import spark.implicits._
      import com.todesking.scalanb.spark.AllImplicits._

      val fs = new LocalFileSystem(tmp.toString)
      val cp = new Checkpoint(fs)

      val vec = fn.udf { i: Int => Vectors.dense(Array(i.toDouble)) }
      val v = cp.source { spark.createDataset(Seq(1, 2, 3)).toDF("value").select('value, vec('value).as("vector")) }

      val c = implicitly[Cacheable[DataFrame]]
      val cv = cp.join(v) { v =>
        c.save(fs, "v2")(v)
        c.load(fs, "v2").get
      }

      cp.join((v, cv)) {
        case (v, cv) =>
          assert(v.as[(Int, Vector)].collect().toSeq == cv.as[(Int, Vector)].collect().toSeq)
      }
    }
  })

  it("should cache MLWritable")(withSpark { implicit spark =>
    withTmpDir { tmp =>
      import spark.implicits._
      import com.todesking.scalanb.spark.AllImplicits._
      val fs = new LocalFileSystem(tmp.toString)
      val cp = new Checkpoint(fs)

      import org.apache.spark.ml
      val c = implicitly[Cacheable[ml.classification.LogisticRegression]]
      val x = cp.source { new ml.classification.LogisticRegression().setRegParam(3.0) }

      val cx =
        cp.join(x) { x =>
          c.save(fs, "cx")(x)
          c.load(fs, "cx").get
        }
      cp.join((x, cx)) {
        case (c, cx) =>
          assert(c.getRegParam == cx.getRegParam)
      }
    }
  })
}

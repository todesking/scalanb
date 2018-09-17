package com.todesking.scalanb.spark.cache

import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Encoder
import org.apache.spark.sql.SparkSession
import scala.reflect.runtime.universe.TypeTag

import com.todesking.scalanb.cache.{ Cacheable, Dep, DepID, CacheFS }
import com.todesking.scalanb.spark.hdfs.HDFS

trait Implicits {
  protected val spark: SparkSession
  import spark.implicits._

  implicit def datasetCacheable[A: Encoder]: Cacheable[Dataset[A]] = new Cacheable[Dataset[A]] {
    private[this] def mkPath(fs: CacheFS, id: DepID): String = {
      s"${fs.protocol}://${fs.path(id)}"
    }
    override def save(fs: CacheFS, d: Dep[Dataset[A]]) = {
      val ds = d.unwrapUNSAFE
      d.unwrapUNSAFE.write.orc(mkPath(fs, d.id))
    }
    override def load(fs: CacheFS, id: DepID) = {
      val p = mkPath(fs, id)
      if (HDFS.exists(p)) Some(Dep.buildUNSAFE(id, spark.read.orc(p).as[A]))
      else None
    }
  }
}

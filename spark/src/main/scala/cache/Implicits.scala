package com.todesking.scalanb.spark.cache

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Encoder
import org.apache.spark.sql.SparkSession

import com.todesking.scalanb.cache.{ Cacheable, Dep, DepID, CacheFS }
import com.todesking.scalanb.spark.hdfs.HDFS

trait Implicits {
  implicit def datasetCacheable[A: Encoder](implicit spark: SparkSession): Cacheable[Dataset[A]] = new Cacheable[Dataset[A]] {
    val dfc = dataFrameCachable
    override def save(fs: CacheFS, d: Dep[Dataset[A]]) =
      dfc.save(fs, Dep.buildUNSAFE(d.id, d.unwrapUNSAFE.toDF))
    override def load(fs: CacheFS, id: DepID) =
      dfc.load(fs, id).map { df => Dep.buildUNSAFE(id, df.unwrapUNSAFE.as[A]) }
  }
  implicit def dataFrameCachable(implicit spark: SparkSession): Cacheable[DataFrame] = new Cacheable[DataFrame] {
    private[this] def mkPath(fs: CacheFS, id: DepID): String = {
      s"${fs.protocol}://${fs.path(id)}"
    }
    override def save(fs: CacheFS, d: Dep[DataFrame]) = {
      val ds = d.unwrapUNSAFE
      d.unwrapUNSAFE.write.orc(mkPath(fs, d.id))
    }
    override def load(fs: CacheFS, id: DepID) = {
      val p = mkPath(fs, id)
      if (HDFS.exists(p)) Some(Dep.buildUNSAFE(id, spark.read.orc(p)))
      else None
    }
  }
}

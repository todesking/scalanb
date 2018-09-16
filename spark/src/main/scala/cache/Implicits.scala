package com.todesking.scalanb.spark.cache

import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Encoder
import org.apache.spark.sql.SparkSession
import scala.reflect.runtime.universe.TypeTag

import com.todesking.scalanb.cache.{ Cacheable, Dep, DepID, IO }
import com.todesking.scalanb.spark.hdfs.HDFS

trait Implicits {
  protected val spark: SparkSession
  import spark.implicits._

  implicit def datasetCacheable[A: Encoder]: Cacheable[Dataset[A]] = new Cacheable[Dataset[A]] {
    private[this] def mkPath(io: IO, id: DepID): String = {
      s"${io.protocol}://${io.path(id)}"
    }
    override def save(io: IO, d: Dep[Dataset[A]]) = {
      val ds = d.unwrapUNSAFE
      d.unwrapUNSAFE.write.orc(mkPath(io, d.id))
    }
    override def load(io: IO, id: DepID) = {
      val p = mkPath(io, id)
      if (HDFS.exists(p)) Some(Dep.buildUNSAFE(id, spark.read.orc(mkPath(io, id)).as[A]))
      else None
    }
  }
}

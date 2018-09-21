package com.todesking.scalanb.spark.cache

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Encoder
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types
import org.apache.spark.ml.linalg.SQLDataTypes

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
    import spark.implicits._

    private[this] def mkPath(fs: CacheFS, id: DepID, name: String): String = {
      s"${fs.protocol}://${fs.path(id)}/$name"
    }
    override def save(fs: CacheFS, d: Dep[DataFrame]) = {
      val df = d.unwrapUNSAFE
      spark.createDataset(serializeSchema(df.schema).split("\n").toSeq)
        .write.text(mkPath(fs, d.id, "schema.json"))
      df.write.orc(mkPath(fs, d.id, "data.orc"))
    }
    override def load(fs: CacheFS, id: DepID) = {
      val p = mkPath(fs, id, "schema.json")
      if (!HDFS.exists(p)) None
      else {
        val schema = deserializeSchema(spark.read.text(mkPath(fs, id, "schema.json")).as[String].collect().mkString("\n"))
        Some(Dep.buildUNSAFE(id, spark.read.schema(schema).orc(mkPath(fs, id, "data.orc"))))
      }
    }

    val simpleTypeToName = {
      import types._
      Map(
        SQLDataTypes.VectorType -> "vector",
        ByteType -> "byte",
        ShortType -> "short",
        IntegerType -> "integer",
        LongType -> "long",
        FloatType -> "float",
        DoubleType -> "double")
    }
    val nameToSimpleType = simpleTypeToName.map { case (k, v) => (v, k) }

    def serializeSchema(t: types.StructType): String = {
      import play.api.libs.json._
      import types._
      import Json.obj

      def ser(t: types.DataType): JsValue = t match {
        case s if simpleTypeToName.contains(s) =>
          obj("type" -> simpleTypeToName(s))
        case StructType(fields) =>
          val jsFields =
            JsArray(fields.map {
              case StructField(k, v, nullable, meta) =>
                obj("name" -> k, "nullable" -> nullable, "type" -> ser(v))
            })
          obj(
            "type" -> "struct",
            "fields" -> jsFields)
        case ArrayType(at, containsNull) =>
          obj("type" -> "array", "subtype" -> ser(at), "containsNull" -> containsNull)
      }

      Json.prettyPrint(ser(t))
    }

    def deserializeSchema(s: String): types.StructType = {
      import play.api.libs.json._
      import types._
      def deserType(t: JsValue): DataType = t match {
        case JsObject(vs) =>
          deserString(vs("type")) match {
            case s if nameToSimpleType.contains(s) =>
              nameToSimpleType(s)
            case "struct" => deserStruct(t)
            case "array" => deserArray(t)
          }
        case _ =>
          throw new RuntimeException(s"Expected data type but $t")
      }
      def deserArray(t: JsValue) = t match {
        case JsObject(vs) if vs.get("type").contains(JsString("array")) =>
          ArrayType(deserType(vs("subtype")), deserBoolean(vs("containsNull")))
      }
      def deserStruct(t: JsValue): StructType = t match {
        case JsObject(vs) if vs.get("type").contains(JsString("struct")) =>
          val fields = vs("fields") match {
            case JsArray(items) =>
              items.map(deserField)
            case _ =>
              throw new RuntimeException(s"Expected field array but ${vs("fields")}")
          }
          StructType(fields)
        case _ =>
          throw new RuntimeException(s"Expected struct but $t")
      }
      def deserField(t: JsValue): StructField = t match {
        case JsObject(o) =>
          val name = deserString(o("name"))
          val tpe = deserType(o("type"))
          val nullable = deserBoolean(o("nullable"))
          StructField(name, tpe, nullable)
        case _ =>
          throw new RuntimeException(s"Expected struct field but $t")
      }
      def deserBoolean(t: JsValue) = t match {
        case JsTrue => true
        case JsFalse => false
        case _ => throw new RuntimeException(s"Expected boolean but $t")
      }
      def deserString(t: JsValue) = t match {
        case JsString(s) => s
        case _ => throw new RuntimeException(s"Expected string but $t")
      }
      deserStruct(Json.parse(s))
    }
  }
}


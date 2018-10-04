package com.todesking.scalanb.spark.cache

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Encoder
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types
import org.apache.spark.ml
import org.apache.spark.ml.linalg.SQLDataTypes

import scala.reflect.runtime.universe.TypeTag

import com.todesking.scalanb.cache.{ Cacheable, Dep, DepID }
import com.todesking.scalanb.io.FileSystem

trait Implicits {
  implicit def datasetCacheable[A: Encoder](implicit spark: SparkSession): Cacheable[Dataset[A]] = new Cacheable[Dataset[A]] {
    val dfc = dataFrameCacheable
    override def save(fs: FileSystem, name: String)(d: Dataset[A]) =
      dfc.save(fs, name)(d.toDF)
    override def load(fs: FileSystem, name: String) =
      dfc.load(fs, name).as[A]
  }

  implicit def dataFrameCacheable(implicit spark: SparkSession): Cacheable[DataFrame] = new Cacheable[DataFrame] {
    override def save(fs: FileSystem, name: String)(df: DataFrame) = {
      val nfs = fs.namespace(name)
      nfs.writeString("schema.json", serializeSchema(df.schema))
      df.cache().write.orc(nfs.uri("data.orc"))
    }
    override def load(fs: FileSystem, name: String) = {
      val nfs = fs.namespace(name)
      val schema = deserializeSchema(nfs.readString("schema.json"))
      spark.read.schema(schema).orc(nfs.uri("data.orc"))
    }

    val simpleTypeToName = {
      import types._
      Map[DataType, String](
        SQLDataTypes.VectorType -> "vector",
        BooleanType -> "boolean",
        ByteType -> "byte",
        ShortType -> "short",
        IntegerType -> "integer",
        LongType -> "long",
        FloatType -> "float",
        DoubleType -> "double",
        StringType -> "string",
        BinaryType -> "binary",
        DateType -> "date",
        CalendarIntervalType -> "calendar_interval",
        TimestampType -> "timestamp",
        NullType -> "null")
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

  // TODO: Use macro to safer usage
  implicit def mlCacheable[A <: ml.util.MLWritable: TypeTag]: Cacheable[A] = new Cacheable[A] {
    val readable = {
      import scala.reflect.runtime.{ currentMirror => cm }
      val companion = implicitly[TypeTag[A]].tpe.typeSymbol.companion.asModule
      cm.reflectModule(companion).instance.asInstanceOf[ml.util.MLReadable[A]]
    }

    override def save(fs: FileSystem, name: String)(value: A) = {
      value.write.save(fs.uri(name, "model"))
    }

    override def load(fs: FileSystem, name: String) = {
      readable.read.load(fs.uri(name, "model")).asInstanceOf[A]
    }
  }

}


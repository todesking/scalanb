package com.todesking.scalanb.cache

import java.time.{ Instant, Duration }

case class MetaData(
  id: DepID,
  typeName: String,
  createdAt: Instant,
  duration: Duration) {
  def toJson: String =
    MetaData.toJsonString(this)
}

object MetaData {
  import play.api.libs.json._
  import Json.obj
  def toJsonString(m: MetaData) = Json.prettyPrint(toJson(m))
  def toJson(m: MetaData): JsValue =
    obj(
      "id" -> serId(m.id),
      "type_name" -> JsString(m.typeName),
      "created_at" -> serInstant(m.createdAt),
      "duration" -> serDuration(m.duration))

  def fromJson(s: String): MetaData =
    fromJson(Json.parse(s))
  def fromJson(v: JsValue): MetaData = v match {
    case JsObject(vs) =>
      MetaData(
        id = deId(vs("id")),
        typeName = deString(vs("type_name")),
        createdAt = deInstant(vs("created_at")),
        duration = deDuration(vs("duration")))
  }

  private[this] def serInstant(i: Instant) =
    obj("epoch_second" -> i.getEpochSecond, "nano" -> i.getNano)
  private[this] def deInstant(o: JsValue) = o match {
    case JsObject(vs) =>
      Instant.ofEpochSecond(
        deLong(vs("epoch_second")),
        deInt(vs("nano")))
  }
  private[this] def serDuration(d: Duration) =
    obj("seconds" -> d.getSeconds, "nano" -> d.getNano)
  private[this] def deDuration(v: JsValue) = v match {
    case JsObject(vs) =>
      Duration.ofSeconds(
        deLong(vs("seconds")),
        deInt(vs("nano")))
  }
  private[this] def serId(id: DepID): JsValue = id match {
    case DepID.Root(ns, name, src, deps) =>
      obj(
        "id_type" -> "root",
        "namespace" -> ns,
        "name" -> name,
        "src" -> src,
        "deps" -> deps.map(serId))
    case DepID.Item(parent, index) =>
      obj(
        "id_type" -> "item",
        "parent" -> serId(parent),
        "index" -> index)
    case DepID.Map(parent, src) =>
      obj(
        "id_type" -> "map",
        "parent" -> serId(parent),
        "src" -> src)
  }
  private[this] def deId(v: JsValue): DepID = v match {
    case JsObject(vs) =>
      vs("id_type") match {
        case JsString("root") =>
          DepID.Root(
            namespace = deString(vs("namespace")),
            name = deString(vs("name")),
            src = deString(vs("src")),
            deps = deSeq(vs("deps")).map(deId))
        case JsString("item") =>
          DepID.Item(
            parent = deId(vs("parent")),
            index = deString(vs("index")))
        case JsString("map") =>
          DepID.Map(
            parent = deId(vs("parent")),
            src = deString(vs("src")))
      }
  }

  private[this] def deSeq(v: JsValue) = v match {
    case JsArray(vs) => vs
  }
  private[this] def deString(v: JsValue) = v match {
    case JsString(s) => s
  }
  private[this] def deLong(o: JsValue) = o match {
    case JsNumber(v) => v.toLong
  }
  private[this] def deInt(o: JsValue) =
    deLong(o).toInt

}


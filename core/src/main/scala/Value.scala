package com.todesking.scalanb

import play.api.libs.json.JsValue
import play.api.libs.json.JsString

// Value representation of various format
case class Value(data: Map[String, JsValue]) {
  def ++(rhs: Value): Value =
    Value(data ++ rhs.data)

  def text: String = data.get("text/plain") match {
    case Some(JsString(s)) => s
    case Some(x) => x.toString
    case None => "???"
  }
}

object Value {
  def isAttachment(mimeType: String) = mimeType match {
    case "text/csv" => true
    case _ => false
  }
  def apply(contentType: String, value: JsValue): Value =
    Value(Map(contentType -> value))

  def apply(contentType: String, value: String): Value =
    apply(contentType, JsString(value))

  def text(s: String): Value =
    apply("text/plain", s)

  def html(s: String): Value =
    apply("text/html", s)

  def csv(s: String): Value =
    apply("text/csv", s)

  def binary(mime: String, data: Array[Byte]): Value =
    apply(mime, JsString(java.util.Base64.getEncoder.encodeToString(data)))

  def svg(s: String): Value =
    apply("image/svg+xml", s)
}


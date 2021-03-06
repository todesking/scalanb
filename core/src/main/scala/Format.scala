package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Output

import scala.reflect.runtime.universe.WeakTypeTag

trait Format[-A] {
  def apply(value: A): Value
  def transform[B](f: B => A): Format[B] = {
    val self = this
    new Format[B] {
      override def apply(value: B) = self.apply(f(value))
    }
  }
}

trait ErrorFormat {
  def apply(t: Throwable): Output.Error
}

object Format {
  def of[A: Format]: Format[A] = implicitly[Format[A]]
  def apply[A](f: A => Value): Format[A] =
    new Format[A] {
      override def apply(v: A) = f(v)
    }

  private[this] def numSummary(d: Double) =
    Seq(1e15 -> "P", 1e12 -> "T", 1e9 -> "G", 1e6 -> "M", 1e3 -> "K")
      .find(_._1 <= d)
      .map { case (n, u) => f"(${d / n}%,.2f$u)" }
      .getOrElse("")

  implicit def defaultAny[A]: Format[A] = new Format[Any] {
    override def apply(value: Any) = Value.text(s"$value")
  }
  implicit val defaultInt: Format[Int] = apply { i =>
    Value.text(f"$i%d${numSummary(i)}")
  }
  implicit val defaultLong: Format[Long] = apply { i =>
    Value.text(f"$i%d${numSummary(i)}")
  }
  implicit val defaultFloat: Format[Float] = apply { i =>
    Value.text(f"$i${numSummary(i)}")
  }
  implicit val defaultDouble: Format[Double] = apply { i =>
    Value.text(f"$i${numSummary(i)}")
  }
  implicit val defaultValue: Format[Value] = apply(identity)

  private[this] def formatSeq[A: Format: WeakTypeTag](xs: Seq[A], collName: String): Value = {
    import format.Table.Col
    val elmName = implicitly[WeakTypeTag[A]].tpe.typeSymbol.name.toString
    format.Table.table(
      Seq(Col(s"$collName[$elmName]", header = true)) +: xs.map(of[A].apply).map { v =>
        Seq(Col(v.text))
      })
  }
  implicit def defaultSeq[A: Format: WeakTypeTag]: Format[Seq[A]] = apply(formatSeq(_, "Seq"))
  implicit def defaultArray[A: Format: WeakTypeTag]: Format[Array[A]] = apply { xs => formatSeq(xs.toSeq, "Array") }

  implicit def defaultMap[A: Format: WeakTypeTag, B: Format: WeakTypeTag]: Format[Map[A, B]] = apply { xs =>
    import format.Table.Col
    val keyName = implicitly[WeakTypeTag[A]].tpe.typeSymbol.name.toString
    val valueName = implicitly[WeakTypeTag[B]].tpe.typeSymbol.name.toString
    format.Table.table(
      Seq(Col(s"Map[$keyName, $valueName]", header = true), Col.Empty) +: xs.toSeq.map {
        case (k, v) =>
          Seq(Col(of[A].apply(k).text), Col(of[B].apply(v).text))
      })
  }
}

object ErrorFormat {
  def buildStackTrace(t: Throwable): Seq[String] = {
    def gather(t: Throwable, prefix: String): Seq[String] = {
      val trace = (prefix + t.toString) +: t.getStackTrace.map { st => s"  ${st.toString}" }
      if (t.getCause == null) trace
      else trace ++ gather(t.getCause, "Caused by: ")
    }
    gather(t, "")
  }
  implicit val default: ErrorFormat = new ErrorFormat {
    override def apply(t: Throwable) = {
      Output.Error(
        "Exception",
        "(Does Jupyter really use this field??)",
        buildStackTrace(t))
    }
  }
}

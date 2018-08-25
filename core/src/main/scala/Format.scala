package com.todesking.scalanb

import com.todesking.scalanb.ipynb.Output

trait Format[-A] {
  def apply(value: A): Value
}

trait ErrorFormat {
  def apply(t: Throwable): Output.Error
}

object Format {
  def apply[A](f: A => Value): Format[A] =
    new Format[A] {
      override def apply(v: A) = f(v)
    }
  implicit def defaultAny[A]: Format[A] = new Format[Any] {
    override def apply(value: Any) = Value.text(s"$value")
  }
  implicit val defaultInt: Format[Int] = apply { i =>
    if (math.abs(i) >= 10000) Value.text(f"$i%,d ($i)")
    else Value.text(f"$i%d")
  }
  implicit val defaultLong: Format[Long] = apply { i =>
    if (math.abs(i) >= 10000) Value.text(f"$i%,d ($i)")
    else Value.text(f"$i%d")
  }
  implicit val defaultFloat: Format[Float] = apply { i =>
    if (math.abs(i) >= 10000.0) Value.text(f"$i%,.2f ($i)")
    else Value.text(f"$i")
  }
  implicit val defaultDouble: Format[Double] = apply { i =>
    if (math.abs(i) >= 10000.0) Value.text(f"$i%,.2f ($i)")
    else Value.text(f"$i")
  }
}

object ErrorFormat {
  implicit val default: ErrorFormat = new ErrorFormat {
    override def apply(t: Throwable) = {
      val stackTraceMessage = t.getStackTrace.map { st =>
        s"  ${st.toString}"
      }
      Output.Error(
        "Exception",
        "(Does Jupyter really use this field??)",
        Seq(t.toString) ++ stackTraceMessage)
    }
  }
}

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
}

object ErrorFormat {
  implicit val default: ErrorFormat = new ErrorFormat {
    override def apply(t: Throwable) = {
      def gather(t: Throwable, prefix: String): Seq[String] = {
        val trace = (prefix + t.toString) +: t.getStackTrace.map { st => s"  ${st.toString}" }
        if (t.getCause == null) trace
        else trace ++ gather(t.getCause, "Caused by: ")
      }
      Output.Error(
        "Exception",
        "(Does Jupyter really use this field??)",
        gather(t, ""))
    }
  }
}

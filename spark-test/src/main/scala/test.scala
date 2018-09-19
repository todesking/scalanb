import com.todesking.{ scalanb => nb }

@nb.spark.Notebook
class SparkNB {
  nb.markdown("# Spark test")

  val df = spark.createDataset(Seq(0.0, 0.00001, 0.002, 0.03, 100)).toDF("v")
  df
  df.count
  df.select('v, ('v + 1).as("vv")).nb.show(100)
}

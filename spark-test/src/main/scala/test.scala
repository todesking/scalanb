import com.todesking.{ scalanb => nb }

@nb.spark.Notebook
class SparkNB {
  nb.markdown("# Spark test")
  import spark.implicits._

  val df = spark.createDataset(Seq(1, 2, 3, 4, 5)).toDF("v")
  df
  df.count
  df.select('v, ('v + 1).as("vv")).nb.show()
}

//this is part of the last step of the QA pipeline
//train a svm with feature-value pairs supplied

package qa.main.ja

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.mllib.classification.SVMWithSGD
import org.apache.spark.mllib.classification.SVMModel
import org.apache.spark.mllib.evaluation.BinaryClassificationMetrics
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.Vectors
import com.databricks.spark.csv
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.sql.types.{ StructType, StructField, StringType, FloatType, IntegerType }

// Load training data in LIBSVM format.

object TrainSpark {

  def answerSelNEval(model: SVMModel, df: org.apache.spark.sql.DataFrame) = {
    val maxScoreRow = df.collect().maxBy(r =>
      {
//for paraAsDoc
        val features = Vectors.dense(r.getInt(2), r.getFloat(3), r.getInt(4), r.getFloat(5), r.getFloat(6), r.getInt(7), r.getInt(8))
//for sectAsDoc
//        val features = Vectors.dense(r.getInt(2), r.getFloat(3), r.getInt(4), r.getFloat(5), r.getFloat(6))
        model.predict(features)
      })
//for paraAsDoc
    maxScoreRow.getInt(7)
//for sectAsDoc
    //    maxScoreRow.getInt(7)
  }

  def getCeiling(df: org.apache.spark.sql.DataFrame) = {
//    for paraAsDoc
    val foundOrNot = df.collect().exists(r => r.getInt(9) == 1)
//  for sectAsDoc
//    val foundOrNot = df.collect().exists(r => r.getInt(7) == 1)
    if (foundOrNot) 1 else 0
  }

  def ifAtAll(qIds: Array[String], df: org.apache.spark.sql.DataFrame) = {
    qIds.map(s => getCeiling(df.filter(s"""qId = "$s""""))).sum
  }

  def best1(model: SVMModel, qIds: Array[String], df: org.apache.spark.sql.DataFrame) = {
    //    import sqlContext.implicits._
    //    val needQId = "NIILC-ECQA2015-00903-01"

    //    df.filter(s"""qId = "$needQId"""").show()
    qIds.map(s => answerSelNEval(model, df.filter(s"""qId = "$s""""))).sum
  }

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local")
    val sc = new SparkContext(conf)
    val sqlContext = new org.apache.spark.sql.SQLContext(sc)
    // Load training data in LIBSVM format.
    val data = MLUtils.loadLibSVMFile(sc, "/home/wailoktam/qa/mylab/training.libsvm")

    // Split data into training (60%) and test (40%).
    //   val splits = data.randomSplit(Array(0.6, 0.4), seed = 11L)
    //   val training = splits(0).cache()
    //   val test = splits(1)

    // Run training algorithm to build the model
    val numIterations = 100
    val model = SVMWithSGD.train(data, numIterations)

    // Clear the default threshold.
    model.clearThreshold()
/**schema for paraAsDoc
*/
    val customSchema = StructType(Array(
      StructField("qId", StringType, true),
      StructField("sentenceId", StringType, true),
      StructField("sentenceNum", IntegerType, true),
      StructField("sentence Lucene score", FloatType, true),
      StructField("sectionNum", IntegerType, true),
      StructField("proximity", FloatType, true),
      StructField("doc Lucene score", FloatType, true),
      StructField("paraNum fr Top", IntegerType, true),
      StructField("paraNum", IntegerType, true),
      StructField("label", IntegerType, true)
    ))

/**
//schema for sectAsDoc
    val customSchema = StructType(Array(
      StructField("qId", StringType, true),
      StructField("sentenceId", StringType, true),
      StructField("sentenceNum", IntegerType, true),
      StructField("sentence Lucene score", FloatType, true),
      StructField("sectionNum", IntegerType, true),
      StructField("proximity", FloatType, true),
      StructField("doc Lucene score", FloatType, true),
      StructField("label", IntegerType, true)
    ))
*/
    val df = sqlContext.read
      .format("com.databricks.spark.csv")
      .option("header", "false") // Use first line of all files as header
      .schema(customSchema)
      .load("/home/wailoktam/qa/mylab/test.csv")

    val qIdRows = df.select("qId").collect().distinct
    val qIds = qIdRows.map(_.getString(0))
    System.err.println("weights" + model.weights.toString())
    System.err.println("qIds" + qIds.length)
    System.err.println("ifAtAll" + ifAtAll(qIds, df))
    System.err.println("best1" + best1(model, qIds, df))

    // Compute raw scores on the test set.
    /**
     * val scoreAndLabels = test.map { point =>
     * val score = model.predict(point.features)
     * System.err.println(score)
     * (score, point.label)
     * }
     *
     * // Get evaluation metrics.
     * val metrics = new BinaryClassificationMetrics(scoreAndLabels)
     * val auROC = metrics.areaUnderROC()
     *
     * println("Area under ROC = " + auROC)
     *
     * // Save and load model
     * model.save(sc, "myModelPath")
     * val sameModel = SVMModel.load(sc, "myModelPath")
     */

    /**
     *
     * val conf = new SparkConf().setAppName("Simple Application").setMaster("local");
     * val sc = new SparkContext(conf)
     * val sqlContext = new org.apache.spark.sql.SQLContext(sc)
     * val data = MLUtils.loadLibSVMFile(sc, "/home/wailoktam/qa/mylab/input/training.csv")
     *
     * // Split data into training (60%) and test (40%).
     * val splits = data.randomSplit(Array(0.6, 0.4), seed = 11L)
     * val training = splits(0).cache()
     * val test = splits(1)
     *
     * // Run training algorithm to build the model
     * val numIterations = 100
     * val model = SVMWithSGD.train(training, numIterations)
     *
     * // Clear the default threshold.
     * model.clearThreshold()
     *
     * // Compute raw scores on the test set.
     * val scoreAndLabels = test.map { point =>
     * val score = model.predict(point.features)
     * (score, point.label)
     * }
     *
     * // Get evaluation metrics.
     * val metrics = new BinaryClassificationMetrics(scoreAndLabels)
     * val auROC = metrics.areaUnderROC()
     *
     * println("Area under ROC = " + auROC)
     */
  }

}

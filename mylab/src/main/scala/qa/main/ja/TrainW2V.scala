package qa.main.ja
import scala.xml._
import org.apache.spark._
import org.apache.spark.rdd._
import org.apache.spark.SparkContext._
import org.apache.spark.mllib.feature.{Word2Vec, Word2VecModel}
import org.apache.spark.streaming.api.java._
import org.apache.spark.{SparkConf, SparkContext}
import org.atilika.kuromoji.Tokenizer
import org.atilika.kuromoji.Token


// Load training data in LIBSVM format.

object TrainW2V {

  def extractSent(fileName:String):List[String] = {
    val tokenizer = Tokenizer.builder.mode(Tokenizer.Mode.NORMAL).build

    val root = XMLLoaderIgnoringDTD.loadFile(fileName)
    val paraSeq = for (para <- root \\ "para") yield {
      (para.child.collect{ case Text(t) => t }).mkString.trim()
    }
    val tokens = tokenizer.tokenize(paraSeq.mkString).toArray
    (tokens.map { t => (t.asInstanceOf[Token].getSurfaceForm()) }).mkString(" ").split("。").toList
  }

  def makeSentenceList(fileNames:Array[String]): List[String] = {
//    def sum3(xs: List[Int]): Int = {
//      if (xs.isEmpty) 0
//    else xs.head + sum3(xs.tail)

      if (fileNames.isEmpty) List()
      else extractSent(fileNames.head) ++ makeSentenceList(fileNames.tail)
  }

//  def train(): Unit =
  def train(sentList: List[String], modelPath: String): Unit = {
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local")
    val sc = new SparkContext(conf)
    val sqlContext = new org.apache.spark.sql.SQLContext(sc)
    System.err.println("sentList"+sentList)
    val input = sentList.map(s => s.split(" ").toSeq)
//      val test = sc.textFile("/home/wailoktam/qa/mylab/input/text8").map(line => line.split(" ").toSeq)
//    System.err.println("class of textfile"+sc.textFile("/home/wailoktam/qa/mylab/input/text8").getClass())
//    System.err.println("class of test"+test.getClass())
//    System.err.println("class of input"+input.getClass())
    val word2vec = new Word2Vec()
    val model = word2vec.fit(sc.parallelize(input))
//    val vectors = model.getVectors("中国").mkString
//    val synonyms = model.findSynonyms("中国", 40)

//    for((synonym, cosineSimilarity) <- synonyms) {
//      println(s"vectors $vectors")
//    }
    model.save(sc, modelPath)
  }

  def main(args: Array[String]): Unit = {


    if (args.length < 2) {
      System.err.println("Usage: scala qa.main.ja.TrainW2V ONEPERPAGE_DIR MODEL_DIR")
      System.exit(1)
    }

    val fileNameList = SharedFunctions.makeFileList(args(0))

    System.err.println("fileNameList"+fileNameList.mkString)
    train(makeSentenceList(fileNameList),args(1))



  }

}
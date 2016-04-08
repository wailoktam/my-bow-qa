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
import java.io.{ FileWriter, BufferedWriter, File }


// Load training data in LIBSVM format.

object TrainW2V {

  def extractSent(xmlFileName:String, bw: java.io.BufferedWriter):Unit= {
    val tokenizer = Tokenizer.builder.mode(Tokenizer.Mode.NORMAL).build

    val root = XMLLoaderIgnoringDTD.loadFile(xmlFileName)
    val paraSeq = for (para <- root \\ "para") yield {
      (para.child.collect{ case Text(t) => t }).mkString.trim()
    }
    val tokens = tokenizer.tokenize(paraSeq.mkString).toArray
    val sentenceLines = (tokens.map { t => (t.asInstanceOf[Token].getSurfaceForm()) }).mkString(" ").split("。").toList
    sentenceLines.map(s=>bw.write(s+"\n"))
  }

  def makeSentenceList(fileNames:Array[String]): String = {
    //    def sum3(xs: List[Int]): Int = {
    //      if (xs.isEmpty) 0
    //    else xs.head + sum3(xs.tail)
    val xml2TxtFile = "/mnt/Works/wailoktam/trainingSentences/segmentedSentenceList.txt"
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(new File(xml2TxtFile)))
    fileNames.map(extractSent(_,bw))
    bw.close()
    xml2TxtFile
    }


/**
 * recursive version
  def makeSentenceList(fileNames:Array[String]): List[String] = {
      if (fileNames.isEmpty) List()
      else extractSent(fileNames.head) ++ makeSentenceList(fileNames.tail)
  }
*/
//  def train(): Unit =
  def trainFirst(sentTxtFile: String, modelPath: String): Unit = {
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local").set( "spark.driver.memory", "80g" )
    val sc = new SparkContext(conf)
    val sqlContext = new org.apache.spark.sql.SQLContext(sc)
    sc.textFile(sentTxtFile, 12).count()
    val input = sc.textFile(sentTxtFile,12).map(line => line.split(" ").toSeq)
//    System.err.println("class of textfile"+sc.textFile("/home/wailoktam/qa/mylab/input/text8").getClass())
//    System.err.println("class of test"+test.getClass())
//    System.err.println("class of input"+input.getClass())
    val word2vec = new Word2Vec()
    val model = word2vec.fit(input)
//    val vectors = model.getVectors("中国").mkString
//    val synonyms = model.findSynonyms("中国", 40)

//    for((synonym, cosineSimilarity) <- synonyms) {
//      println(s"vectors $vectors")
//    }
    model.save(sc, modelPath)
  }

  def trainSecond(modelPath: String): Unit = {
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local")
    val sc = new SparkContext(conf)
    val sqlContext = new org.apache.spark.sql.SQLContext(sc)
    val input = sc.textFile("/mnt/Works/wailoktam/trainingSentences/segmentedSentenceList.txt",12).map(line => line.split(" ").toSeq)
    val word2vec = new Word2Vec()
    val model = word2vec.fit(input)
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
    trainFirst(makeSentenceList(fileNameList),args(1))
//    trainSecond(args(1))
  }

}
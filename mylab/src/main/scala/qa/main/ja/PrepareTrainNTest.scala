package qa.main.ja

import java.io.{FileWriter, BufferedWriter, File}

import org.apache.lucene.index.{IndexReader, Term, DirectoryReader}
import org.apache.lucene.search.{TermQuery, IndexSearcher}
import org.apache.lucene.store.FSDirectory
import scala.math._
import scala.util.matching.Regex
import scala.xml._
import org.atilika.kuromoji.Tokenizer
import org.atilika.kuromoji.Token
import com.ibm.icu.text.Normalizer2

object PrepareTrainMain {

  def myNormalize(input: String): String = {
    val tokenizer = Tokenizer.builder.mode(Tokenizer.Mode.NORMAL).build
    val tokens = tokenizer.tokenize(input).toArray
    val normalizer = Normalizer2.getNFCInstance()
    (tokens.map { t => normalizer.normalize(t.asInstanceOf[Token].getSurfaceForm()) }).mkString
  }

  def sectPara(id: String): Int = {
    System.err.println(s"id in sectPara ${id.trim()}")
    val paraIDRe = """(?<=\/)\d+$""".r
    paraIDRe.findFirstIn(id.trim()).getOrElse("0").toInt
  }

  def sectSect(id: String): Int = {
    System.err.println(s"id in sectSect ${id.trim()}")
    val sectIDRe = """(?<=s)\d+""".r
    System.err.println(s"output in sectSect ${sectIDRe.findFirstIn(id.trim()).getOrElse("0").toInt}")
    sectIDRe.findFirstIn(id.trim()).getOrElse("0").toInt
  }

  def getParasOfNext(idNum: Int, oldID: String, index_searcher: IndexSearcher): Int = {
    //idNum = 2
    //oldID = 2:450-4-1-1-4
    val newID = oldID + "s" + idNum.toString()
    val term = new Term("id", newID)
    //newID = 2:450-4-1-1-4-2
    System.err.println(s"old id + s num ${oldID + "s" + idNum.toString()}")
    val query = new TermQuery(term)
    if ((index_searcher.search(query, 1).scoreDocs).length == 0) 0
    else {
      val paraSelfCount = for (scoreWDoc <- index_searcher.search(query, 1).scoreDocs) yield {
        index_searcher.doc(scoreWDoc.doc).get("paras")
      }
      val paraCount = paraSelfCount.mkString.toInt + getParasOfDesc(idNum, oldID, index_searcher)
      System.err.println(s"newID in getParasOfNext ${newID}")
      System.err.println(s"paraSelfCount in getParasOfNext ${paraSelfCount.mkString.toInt}")
      System.err.println(s"paraCount in getParasOfNext ${paraCount}")
      paraCount + getParasOfNext(idNum + 1, oldID, index_searcher)
    }
  }

  def getParasOfDesc(idNum: Int, oldID: String, index_searcher: IndexSearcher): Int = {
    //idNum = 4
    //oldID = 2:450-4-1-1

    val newID = oldID + "s" + idNum.toString()
    //newID = 2:450-4-1-1-4
    val term = new Term("id", newID + "s" + "1")
    System.err.println(s"newID plus s1  in getParasOfDesc ${newID + "s" + "1"}")
    val query = new TermQuery(term)
    if ((index_searcher.search(query, 1).scoreDocs).length == 0) 0
    else {
      val paraSelfCount = for (scoreWDoc <- index_searcher.search(query, 1).scoreDocs) yield {
        index_searcher.doc(scoreWDoc.doc).get("paras")
      }
      val paraCount = paraSelfCount.mkString.toInt + getParasOfNext(2, newID, index_searcher)

      System.err.println(s"paraSelfCount in getParasOfDesc ${paraSelfCount.mkString.toInt}")
      System.err.println(s"paraCount in getParasOfDesc ${paraCount}")
      paraCount + getParasOfDesc(1, newID, index_searcher)
    }
  }


  def getParasOfPrev(idNum: Int, oldID: String, indexSearcher: IndexSearcher): Int = {
    //idNum = 4
    //oldID = 2:450-4-1-1
    if (idNum == 0) 0
    else {
      val newID = oldID + "s" + idNum.toString()
      //newID = 2:450-4-1-1-4
      val term = new Term("id", newID)
      System.err.println(s"newID in getParasOfPrev ${newID}")
      val query = new TermQuery(term)
      val paraSelfCount = for (scoreWDoc <- indexSearcher.search(query, 1).scoreDocs) yield {
        indexSearcher.doc(scoreWDoc.doc).get("paras")
      }
      System.err.println(s"paraSelfCount in getParasOfPrev ${paraSelfCount.mkString}")
      val paraCount = paraSelfCount.mkString.toInt + getParasOfDesc(idNum, oldID, indexSearcher)

      System.err.println(s"paraSelfCount in getParasOfPrev ${paraSelfCount.mkString.toInt}")
      System.err.println(s"paraCount in getParasOfPrev ${paraCount}")
      paraCount + getParasOfPrev(idNum - 1, oldID, indexSearcher)
    }
  }


  def wholePara(id: String, index_searcher: IndexSearcher): Int = {
    val sectIDRe = """(?<=s)(\d+)(?=[\D]*)$""".r
    val sectIDwSepRe = """s(\d+)(?=[\D]*)$""".r
    val splitIDOpt = sectIDRe.findFirstIn(id.trim())
    val newID = sectIDwSepRe.replaceFirstIn(id.trim(), "")
    System.err.println(s"newIDInwholePara ${newID}")

    System.err.println(s"id in wholePara ${id}")
    val term = new Term("id", id.trim())
    val query = new TermQuery(term)

    System.err.println(s"splitIDOpt in wholePara ${splitIDOpt}")
    splitIDOpt match {
      case None => {

        System.err.println(s"nonecase in wholePara${(for (scoreWDoc <- index_searcher.search(query, 1).scoreDocs) yield index_searcher.doc(scoreWDoc.doc).get("paras")).mkString.toInt}")
        (for (scoreWDoc <- index_searcher.search(query, 1).scoreDocs) yield index_searcher.doc(scoreWDoc.doc).get("paras")).mkString.toInt
      }
      case Some(splitID) => {
        getParasOfPrev(splitID.toInt - 1, newID, index_searcher) + wholePara(newID, index_searcher)
      }
    }
  }

  //for (token <- (questionTokens \\ "token")) yield {
  //  token match {
  //    case tokenRe(token) =>


  //  }
  //}


  def proximity(sentence: String, question: String, index_reader: IndexReader): Double = {
    val tokenizer = Tokenizer.builder.mode(Tokenizer.Mode.NORMAL).build
    val questionTokens = tokenizer.tokenize(question).toArray
    System.err.println(s"question ${question}")
    System.err.println(s"sentence ${sentence}")
    val qInS = questionTokens.map(t => {
      System.err.println(s"t in qInS ${t.asInstanceOf[Token].getSurfaceForm()}")
      val surfaceForm = t.asInstanceOf[Token].getSurfaceForm()
      val term = new Term("text", surfaceForm)
      val weight = 1 + log10(index_reader.numDocs() / (index_reader.docFreq(term) + 1))
      if (sentence.contains(surfaceForm)) weight else 0
    }).sum
    val sentenceTokens = tokenizer.tokenize(sentence).toArray
    val kNotInQ = sentenceTokens.map(t => {
      val surfaceForm = t.asInstanceOf[Token].getSurfaceForm()
      val term = new Term("text", surfaceForm)
      val weight = 1 + log10(index_reader.numDocs() / (index_reader.docFreq(term) + 1))
      System.err.println(s"t in kNotInQ ${t.asInstanceOf[Token].getSurfaceForm()}")
      if (question.contains(surfaceForm)) 0 else weight
    }).sum
    qInS - kNotInQ
  }

/**
 * <value 1>...<value n><label>
  def checkQuestionAndAnnotationPara(questionXML: Node, indexDirName: String, csvBw: BufferedWriter): Unit = {
    val sectIndexDir = FSDirectory.open(new File(indexDirName + "/sect"))
    val paraIndexDir = FSDirectory.open(new File(indexDirName + "/para"))
    val sentIndexDir = FSDirectory.open(new File(indexDirName + "/sent"))
    val sectIndexReader = DirectoryReader.open(sectIndexDir)
    val sectIndexSearcher = new IndexSearcher(sectIndexReader)
    val paraIndexReader = DirectoryReader.open(paraIndexDir)
    val paraIndexSearcher = new IndexSearcher(paraIndexReader)
    val sentIndexReader = DirectoryReader.open(sentIndexDir)
    val sentIndexSearcher = new IndexSearcher(sentIndexReader)
    for (doc <- (questionXML \\ "doc")) yield {
      val sectSectNum = sectSect((doc \\ "did").text)
      val sectParaNum = sectPara((doc \\ "did").text)
      System.err.println(s"sectParaOut ${sectParaNum}")


      //        val sectParaNum = if (docType == "para") {
      //          sectPara((doc \\ "did").text)
      //        }
      //         else {
      //         "0"
      //        }
      //doc\\"did".text = 2:450-4-1-1-5/9
      val paraIDRe = """r\d+""".r
      val idWoPara = paraIDRe.replaceAllIn((doc \\ "did").text, "")
      //idWoPara = 2:450-4-1-1-5
      val sectIDRe = """(?<=s)(\d+)(?=[\D]*)$""".r
      val sectIDwSepRe = """s(\d+)(?=[\D]*)$""".r
      val splitIDOpt = sectIDRe.findFirstIn(idWoPara.trim())
      //splitIDOpt = Some(5)
      val idWoLastSect = sectIDwSepRe.replaceFirstIn(idWoPara.trim(), "")
      val paraFrTop = splitIDOpt match {
        case None => {
          sectParaNum.toString()
          //            sectParaNum.toString() ::  List(label.toString())
        }
        case Some(splitID) => {
          if ((splitID.toInt == 0) == false) {
            System.err.println(s"splitIDInCheckQ&A ${splitID}")
            //              val temp1 = getParasOfPrev(splitID.toInt - 1, idWoLastSect, index_searcher)
            //              val temp2 = wholePara(idWoLastSect, index_searcher)
            //              System.err.println(s"temp1InCheckQ&A ${temp1}")
            //              System.err.println(s"temp2InCheckQ&A ${temp2}")
            //              temp1 + temp2 :: featLast1
            //              sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, index_searcher) + wholePara(idWoLastSect, index_searcher) :: List(label.toString())
            sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, sectIndexSearcher) + wholePara(idWoLastSect, sectIndexSearcher)
          }
          else 0
        }
      }
      System.err.println(s"idWoParaInCheckQ&A ${idWoPara}")
      System.err.println(s"idWoLastSectInCheckQ&A ${idWoLastSect}")
      System.err.println(s"splitIDinCheckQ&A ${splitIDOpt}")
      //        for (answer <- questionXML \\ "answer") yield {
      //          if (normalizedText.contains(myNormalize(answer.text.trim()))) println ("get")
      //        }
      //        val label = "false"
      //val sentenceCounterStream = Stream.iterate(1)(_ + 1).iterator
      for (sentence <- (doc \\ "sent")) yield {
        //          System.err.println(s"doc ${doc}")
        val sentText = myNormalize((sentence \\ "stext").text)
        //val sentenceCounter = sentenceCounterStream.next
        //          val sentenceID = (doc \\ "did").text+"^"+sentenceCounter
        //          System.err.println(s"sentenceCounter ${sentenceCounter}")
        val label = {
          if ((questionXML \\ "answer") exists { a: Node => sentText.contains(myNormalize(a.text.trim())) }) 1 else 0
        }
        System.err.println(s"sentence in checkQ&A${sentence}")
        //          for (answer<-(questionXML \\ "answer")) System.err.println(s"answer in checkQ&A${myNormalize(answer.text.trim())}")
        //          val maxProximitySentence  = ((doc \\ "dtext").text.trim().split("。"))maxBy(proximity(_, (questionXML\"text").text, index_reader))
        val sentenceProximity = proximity(sentText, (questionXML \ "text").text, sentIndexReader)



        val sentenceNum = (sentence \\ "scount").text.trim()
        val featLast1 = sectParaNum.toString() :: List(label.toString())

        //idWoLastSect = 2:450-4-1-1
        val featLast2 = paraFrTop :: featLast1
        //            else 0 :: List(label.toString())
        val featLast3 = (doc \\ "dscore").text.trim() :: featLast2
        val featLast4 = sentenceProximity :: featLast3
        val featLast5 = sectSectNum :: featLast4
        val featLast6 = (sentence \\ "sscore").text.trim() :: featLast5
        val featLast7 = sentenceNum :: featLast6
        System.err.println(s"featLast4 ${featLast5}")
        val bwLine = (doc \\ "did").text.trim()+"t"+sentenceNum :: featLast7
        csvBw.write(bwLine.mkString(",") + "\n")
      }

    }
  }
*/

def checkQuestionAndAnnotationPara(questionXML: Node, indexDirName: String, csvBw: BufferedWriter): Unit = {
  val sectIndexDir = FSDirectory.open(new File(indexDirName + "/sect"))
  val paraIndexDir = FSDirectory.open(new File(indexDirName + "/para"))
  val sentIndexDir = FSDirectory.open(new File(indexDirName + "/sent"))
  val sectIndexReader = DirectoryReader.open(sectIndexDir)
  val sectIndexSearcher = new IndexSearcher(sectIndexReader)
  val paraIndexReader = DirectoryReader.open(paraIndexDir)
  val paraIndexSearcher = new IndexSearcher(paraIndexReader)
  val sentIndexReader = DirectoryReader.open(sentIndexDir)
  val sentIndexSearcher = new IndexSearcher(sentIndexReader)
  for (doc <- (questionXML \\ "doc")) yield {
    val sectSectNum = sectSect((doc \\ "did").text)
    val sectParaNum = sectPara((doc \\ "did").text)
    System.err.println(s"sectParaOut ${sectParaNum}")


    //        val sectParaNum = if (docType == "para") {
    //          sectPara((doc \\ "did").text)
    //        }
    //         else {
    //         "0"
    //        }
    //doc\\"did".text = 2:450-4-1-1-5/9
    val paraIDRe = """r\d+""".r
    val idWoPara = paraIDRe.replaceAllIn((doc \\ "did").text, "")
    //idWoPara = 2:450-4-1-1-5
    val sectIDRe = """(?<=s)(\d+)(?=[\D]*)$""".r
    val sectIDwSepRe = """s(\d+)(?=[\D]*)$""".r
    val splitIDOpt = sectIDRe.findFirstIn(idWoPara.trim())
    //splitIDOpt = Some(5)
    val idWoLastSect = sectIDwSepRe.replaceFirstIn(idWoPara.trim(), "")
    val paraFrTop = splitIDOpt match {
      case None => {
        sectParaNum.toString()
        //            sectParaNum.toString() ::  List(label.toString())
      }
      case Some(splitID) => {
        if ((splitID.toInt == 0) == false) {
          System.err.println(s"splitIDInCheckQ&A ${splitID}")
          //              val temp1 = getParasOfPrev(splitID.toInt - 1, idWoLastSect, index_searcher)
          //              val temp2 = wholePara(idWoLastSect, index_searcher)
          //              System.err.println(s"temp1InCheckQ&A ${temp1}")
          //              System.err.println(s"temp2InCheckQ&A ${temp2}")
          //              temp1 + temp2 :: featLast1
          //              sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, index_searcher) + wholePara(idWoLastSect, index_searcher) :: List(label.toString())
          sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, sectIndexSearcher) + wholePara(idWoLastSect, sectIndexSearcher)
        }
        else 0
      }
    }
    System.err.println(s"idWoParaInCheckQ&A ${idWoPara}")
    System.err.println(s"idWoLastSectInCheckQ&A ${idWoLastSect}")
    System.err.println(s"splitIDinCheckQ&A ${splitIDOpt}")
    //        for (answer <- questionXML \\ "answer") yield {
    //          if (normalizedText.contains(myNormalize(answer.text.trim()))) println ("get")
    //        }
    //        val label = "false"
    //val sentenceCounterStream = Stream.iterate(1)(_ + 1).iterator
    for (sentence <- (doc \\ "sent")) yield {
      //          System.err.println(s"doc ${doc}")
      val sentText = myNormalize((sentence \\ "stext").text)
      //val sentenceCounter = sentenceCounterStream.next
      //          val sentenceID = (doc \\ "did").text+"^"+sentenceCounter
      //          System.err.println(s"sentenceCounter ${sentenceCounter}")
      val label = {
        if ((questionXML \\ "answer") exists { a: Node => sentText.contains(myNormalize(a.text.trim())) }) 1 else 0
      }
      System.err.println(s"sentence in checkQ&A${sentence}")
      //          for (answer<-(questionXML \\ "answer")) System.err.println(s"answer in checkQ&A${myNormalize(answer.text.trim())}")
      //          val maxProximitySentence  = ((doc \\ "dtext").text.trim().split("。"))maxBy(proximity(_, (questionXML\"text").text, index_reader))
      val sentenceProximity = proximity(sentText, (questionXML \ "text").text, sentIndexReader)



      val sentenceNum = (sentence \\ "scount").text.trim()
      val featLast1 = "1:"::List(sectParaNum.toString())

      //idWoLastSect = 2:450-4-1-1
      val featLast2 = "2:" :: paraFrTop :: " " :: featLast1
      //            else 0 :: List(label.toString())
      val featLast3 = "3:" ::(doc \\ "dscore").text.trim()  :: " " :: featLast2
      val featLast4 = "4:" :: sentenceProximity :: " " :: featLast3
      val featLast5 = "5:" :: sectSectNum :: " " :: featLast4
      val featLast6 = "6:" :: (sentence \\ "sscore").text.trim() :: " " :: featLast5
      val featLast7 = "7:" :: sentenceNum :: " " :: featLast6
      val allFeats = featLast7.mkString
      csvBw.write(label.toString()+" "+allFeats +  " # " + (doc \\ "did").text.trim()+"t"+sentenceNum + "\n")
/**
      val featLast1 = "1"::List(sectParaNum.toString())

      //idWoLastSect = 2:450-4-1-1
      val featLast2 = "2" :: paraFrTop :: featLast1
      //            else 0 :: List(label.toString())
      val featLast3 = "3" ::(doc \\ "dscore").text.trim() :: featLast2
      val featLast4 = "4" :: sentenceProximity :: featLast3
      val featLast5 = "5" :: sectSectNum :: featLast4
      val featLast6 = "6" :: (sentence \\ "sscore").text.trim() :: featLast5
      val featLast7 = "7" :: sentenceNum :: featLast6
      val allFeats = featLast7.mkString(":")
      csvBw.write(label.toString()+" "+allFeats + "\n")
*/
    }

  }
}

  def apply(xmlWDocs: Node, indexDirName: String, csvFile: BufferedWriter, docType: String) = {
    docType match {
      case "paragraph" => {
//        for libsvm
//        csvFile.write("id, sent num in para, sent lucene score, section, proximity, doc lucene score, para from top, para in section,label\n")
        (xmlWDocs \\ "question") map (checkQuestionAndAnnotationPara(_, indexDirName, csvFile))
      }


    }
  }
}
object PrepareTrainNTest {
  def main(args: Array[String]): Unit = {
    if (args.length < 4) {
      System.err.println("Usage: scala qa.main.ja.PrepareTrain INPUT_XML INDEX_DIR DOC_TYPE TRAINTOVERTEST")
      System.exit(1)
    }

    val trainFile = new File("training.csv")
    val trainBw = new BufferedWriter(new FileWriter(trainFile))
    val testFile = new File("test.csv")
    val testBw = new BufferedWriter(new FileWriter(testFile))
//    csvBw.write("id,para from top,label\n")

    PrepareTrainMain(XMLLoaderIgnoringDTD.loadFile(args(0)),args(1), trainBw, testBw, args(2), args(3))
    trainBw.close()
    testBw.close()
  }
}


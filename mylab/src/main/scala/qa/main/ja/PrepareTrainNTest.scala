//This is part of the last step in the BOW QA pipeline 
//ranking sentences in a doc

package qa.main.ja

import java.io.{ FileWriter, BufferedWriter, File }

import org.apache.lucene.analysis.ja.JapaneseAnalyzer
import org.apache.lucene.index.{ IndexReader, Term, DirectoryReader }
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{ TermQuery, IndexSearcher }
import org.apache.lucene.store.FSDirectory
import scala.math._
import scala.util.matching.Regex
import scala.xml._
import org.atilika.kuromoji.Tokenizer
import org.atilika.kuromoji.Token
import com.ibm.icu.text.Normalizer2

object PrepareTrainNTestMain {

  def sectParaFrParaId(id: String): Int = {
    System.err.println(s"id in sectPara ${id.trim()}")
    val paraIDRe = """(?<=r)\d+$""".r
    paraIDRe.findFirstIn(id.trim()).getOrElse("0").toInt
  }


  def sectParaFrSentId(id: String): Int = {
    System.err.println(s"id in sectPara ${id.trim()}")
    val sentIDRe = """t\d+""".r
    val idWoSent = sentIDRe.replaceAllIn(id,"")
    System.err.println(s"idWoSent in sectPara ${id.trim()}")
    val paraIDRe = """(?<=r)\d+$""".r
    paraIDRe.findFirstIn(id.trim()).getOrElse("0").toInt
  }

  def sectSect(id: String): Int = {
//mainsection
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



//score for measuring how close the keywords are to each other
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

 //prepare training data (feature-value pairs) in the case of treating a sect as a doc
  def prepareTrainforSectAsDoc(questionXML: Node, indexDirName: String, trainBw: BufferedWriter): Unit = {
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




      for (sentence <- (doc \\ "sent")) yield {
        //          System.err.println(s"doc ${doc}")
        val sentText = SharedFunctions.myNormalize(org.apache.lucene.queryparser.classic.QueryParserBase.escape(SharedFunctions.powerTrim((sentence \\ "stext").text)))
        System.err.println("sentTex"+sentText)
        val analyzer = new JapaneseAnalyzer
        val parser = new QueryParser("text", analyzer)

        val query = parser.parse(sentText)
        val sentenceID =
          (for (scoreWDoc <- sentIndexSearcher.search(query, 1).scoreDocs) yield {
            sentIndexSearcher.doc(scoreWDoc.doc).get("id")
          }).mkString
        System.err.println("sentenceID"+sentenceID)
        val sectParaNum = sectParaFrSentId(sentenceID)
        val sentIDRe = """t\d+""".r
        val idWoSent = sentIDRe.replaceAllIn(sentenceID,"")

        val paraIDRe = """r\d+""".r
        val idWoPara = paraIDRe.replaceAllIn(idWoSent, "")
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
              sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, sectIndexSearcher) + wholePara(idWoLastSect, sectIndexSearcher)
            } else 0
          }
        }

  
        val label = {
          if ((questionXML \\ "answer") exists { a: Node => sentText.contains(SharedFunctions.myNormalize(a.text.trim())) }) 1 else 0
        }
        System.err.println(s"sentence in checkQ&A${sentence}")
        val sentenceProximity = proximity(sentText, (questionXML \ "text").text, sentIndexReader)

        val sentenceNum = (sentence \\ "scount").text.trim()

        val featLast1 = "7:" :: List(sectParaNum.toString())

        //idWoLastSect = 2:450-4-1-1
        val featLast2 = "6:" :: paraFrTop :: " " :: featLast1
        //            else 0 :: List(label.toString())

        val featLast3 = "5:" :: (doc \\ "dscore").text.trim() :: " " ::featLast2
        val featLast4 = "4:" :: sentenceProximity :: " " :: featLast3
        val featLast5 = "3:" :: sectSectNum :: " " :: featLast4
        val featLast6 = "2:" :: (sentence \\ "sscore").text.trim() :: " " :: featLast5
        val featLast7 = "1:" :: sentenceNum :: " " :: featLast6
        val allFeats = featLast7.mkString
        trainBw.write(label.toString() + " " + allFeats + "\n")
      }
    }
  }



 //prepare training data (feature-value pairs) in the case of treating a para as a doc
  def prepareTrainforParaAsDoc(questionXML: Node, indexDirName: String, trainBw: BufferedWriter): Unit = {
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
      val sectParaNum = sectParaFrParaId((doc \\ "did").text)
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
      System.err.println("testregular"+idWoPara)
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
            sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, sectIndexSearcher) + wholePara(idWoLastSect, sectIndexSearcher)
          } else 0
        }
      }
      System.err.println(s"idWoParaInCheckQ&A ${idWoPara}")
      System.err.println(s"idWoLastSectInCheckQ&A ${idWoLastSect}")
      System.err.println(s"splitIDinCheckQ&A ${splitIDOpt}")
      for (sentence <- (doc \\ "sent")) yield {
        //          System.err.println(s"doc ${doc}")
        val sentText = SharedFunctions.myNormalize((sentence \\ "stext").text)
     val label = {
          if ((questionXML \\ "answer") exists { a: Node => sentText.contains(SharedFunctions.myNormalize(a.text.trim())) }) 1 else 0
        }
        System.err.println(s"sentence in checkQ&A${sentence}")
       val sentenceProximity = proximity(sentText, (questionXML \ "text").text, sentIndexReader)

        val sentenceNum = (sentence \\ "scount").text.trim()
        val featLast1 = "7:" :: List(sectParaNum.toString())

        //idWoLastSect = 2:450-4-1-1
        val featLast2 = "6:" :: paraFrTop :: " " :: featLast1
        //            else 0 :: List(label.toString())
        val featLast3 = "5:" :: (doc \\ "dscore").text.trim() :: " " :: featLast2
        val featLast4 = "4:" :: sentenceProximity :: " " :: featLast3
        val featLast5 = "3:" :: sectSectNum :: " " :: featLast4
        val featLast6 = "2:" :: (sentence \\ "sscore").text.trim() :: " " :: featLast5
        val featLast7 = "1:" :: sentenceNum :: " " :: featLast6
        val allFeats = featLast7.mkString
        trainBw.write(label.toString() + " " + allFeats + "\n")
      }
    }
  }

 //prepare test data (feature-value pairs) in the case of treating a sect as a doc
  def prepareTestforSectAsDoc(questionXML: Node, indexDirName: String, testBw: BufferedWriter): Unit = {
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

//      val sentenceCounterStream = Stream.iterate(1)(_ + 1).iterator
      for (sentence <- (doc \\ "sent")) yield {
        //          System.err.println(s"doc ${doc}")
        val sentText = SharedFunctions.myNormalize(org.apache.lucene.queryparser.classic.QueryParserBase.escape(SharedFunctions.powerTrim((sentence \\ "stext").text)))
        val analyzer = new JapaneseAnalyzer
        val parser = new QueryParser("text", analyzer)
        ///
        //        val term = new Term("text", sentText)
        //newID = 2:450-4-1-1-4-2
        //        System.err.println(s"old id + s num ${oldID + "s" + idNum.toString()}")
        //        val query = new TermQuery(term)
        val query = parser.parse(sentText)


        val sentenceID =
          (for (scoreWDoc <- sentIndexSearcher.search(query, 1).scoreDocs) yield {
            sentIndexSearcher.doc(scoreWDoc.doc).get("id")
          }).mkString
        val sectParaNum = sectParaFrSentId(sentenceID)
        val sentIDRe = """t\d+""".r
        val idWoSent = sentIDRe.replaceAllIn(sentenceID,"")

        val paraIDRe = """r\d+""".r
        val idWoPara = paraIDRe.replaceAllIn(idWoSent, "")
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
              sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, sectIndexSearcher) + wholePara(idWoLastSect, sectIndexSearcher)
            } else 0
          }
        }

        //          val sentenceID = (doc \\ "did").text+"^"+sentenceCounter
        //          System.err.println(s"sentenceCounter ${sentenceCounter}")

        val label = {
          if ((questionXML \\ "answer") exists { a: Node => sentText.contains(SharedFunctions.myNormalize(a.text.trim())) }) 1 else 0
        }
        System.err.println(s"sentence in checkQ&A${sentence}")
        //          for (answer<-(questionXML \\ "answer")) System.err.println(s"answer in checkQ&A${myNormalize(answer.text.trim())}")
        //          val maxProximitySentence  = ((doc \\ "dtext").text.trim().split("。"))maxBy(proximity(_, (questionXML\"text").text, index_reader))
        val sentenceProximity = proximity(sentText, (questionXML \ "text").text, sentIndexReader)

        val sentenceNum = (sentence \\ "scount").text.trim()
        val featLast1 = sectParaNum.toString() :: List(label)

        //idWoLastSect = 2:450-4-1-1
        val featLast2 = paraFrTop :: featLast1
        //            else 0 :: List(label.toString())
        val featLast3 = (doc \\ "dscore").text.trim() ::featLast2
        val featLast4 = sentenceProximity :: featLast3
        val featLast5 = sectSectNum :: featLast4
        val featLast6 = (sentence \\ "sscore").text.trim() :: featLast5
        val featLast7 = sentenceNum :: featLast6
        val testLine = (questionXML \ "@id").text.trim() :: sentenceID :: featLast7
        testBw.write(testLine.mkString(",") + "\n")
 
      }
    }
  }

 //prepare test data (feature-value pairs) in the case of treating a para as a doc  
  
  def prepareTestforParaAsDoc(questionXML: Node, indexDirName: String, testBw: BufferedWriter): Unit = {
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
      val sectParaNum = sectParaFrParaId((doc \\ "did").text)
      System.err.println(s"sectParaOut ${sectParaNum}")


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
             sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, sectIndexSearcher) + wholePara(idWoLastSect, sectIndexSearcher)
          } else 0
        }
      }
      System.err.println(s"idWoParaInCheckQ&A ${idWoPara}")
      System.err.println(s"idWoLastSectInCheckQ&A ${idWoLastSect}")
      System.err.println(s"splitIDinCheckQ&A ${splitIDOpt}")
      //        for (answer <- questionXML \\ "answer") yield {
      //          if (normalizedText.contains(myNormalize(answer.text.trim()))) println ("get")
      //        }
      //        val label = "false"
      val sentenceCounterStream = Stream.iterate(1)(_ + 1).iterator
      for (sentence <- (doc \\ "sent")) yield {
        //          System.err.println(s"doc ${doc}")
        val sentText = SharedFunctions.myNormalize((sentence \\ "stext").text)
        val sentenceCounter = sentenceCounterStream.next
        //          val sentenceID = (doc \\ "did").text+"^"+sentenceCounter
        //          System.err.println(s"sentenceCounter ${sentenceCounter}")

        val label = {
          if ((questionXML \\ "answer") exists { a: Node => sentText.contains(SharedFunctions.myNormalize(a.text.trim())) }) 1 else 0
        }
        System.err.println(s"sentence in checkQ&A${sentence}")
        //          for (answer<-(questionXML \\ "answer")) System.err.println(s"answer in checkQ&A${myNormalize(answer.text.trim())}")
        //          val maxProximitySentence  = ((doc \\ "dtext").text.trim().split("。"))maxBy(proximity(_, (questionXML\"text").text, index_reader))
        val sentenceProximity = proximity(sentText, (questionXML \ "text").text, sentIndexReader)

        val sentenceNum = (sentence \\ "scount").text.trim()
        val featLast1 = sectParaNum.toString() :: List(label)

        //idWoLastSect = 2:450-4-1-1
        val featLast2 = paraFrTop :: featLast1
        //            else 0 :: List(label.toString())
        val featLast3 = (doc \\ "dscore").text.trim() :: featLast2
        val featLast4 = sentenceProximity :: featLast3
        val featLast5 = sectSectNum :: featLast4
        val featLast6 = (sentence \\ "sscore").text.trim() :: featLast5
        val featLast7 = sentenceNum :: featLast6
        val testLine = (questionXML \ "@id").text.trim() :: (doc \\ "did").text.trim() + "t" + sentenceCounter :: featLast7
        testBw.write(testLine.mkString(",") + "\n")

      }
    }
  }

  def apply(xmlWDocs: Node, indexDirName: String, trainFile: BufferedWriter, testFile: BufferedWriter, docType: String, trainOverTest: Int) = {
    docType match {
      case "paragraph" => {
        val trainTestPair = (xmlWDocs \\ "question").splitAt(800 / (trainOverTest + 1) * trainOverTest - 1)
        //        for libsvm
        //        csvFile.write("id, sent num in para, sent lucene score, section, proximity, doc lucene score, para from top, para in section,label\n")
        trainTestPair._1 map (prepareTrainforParaAsDoc(_, indexDirName, trainFile))
        trainTestPair._2 map (prepareTestforParaAsDoc(_, indexDirName, testFile))
      }
      case "section" => {
        val trainTestPair = (xmlWDocs \\ "question").splitAt(800 / (trainOverTest + 1) * trainOverTest - 1)
        //        for libsvm
        //        csvFile.write("id, sent num in para, sent lucene score, section, proximity, doc lucene score, para from top, para in section,label\n")
        trainTestPair._1 map (prepareTrainforSectAsDoc(_, indexDirName, trainFile))
        trainTestPair._2 map (prepareTestforSectAsDoc(_, indexDirName, testFile))
      }
      case "page" => {
        val trainTestPair = (xmlWDocs \\ "question").splitAt(800 / (trainOverTest + 1) * trainOverTest - 1)
        //        for libsvm
        //        csvFile.write("id, sent num in para, sent lucene score, section, proximity, doc lucene score, para from top, para in section,label\n")
        trainTestPair._1 map (prepareTrainforSectAsDoc(_, indexDirName, trainFile))
        trainTestPair._2 map (prepareTestforSectAsDoc(_, indexDirName, testFile))
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

    val trainFile = new File("training.libsvm")
    val trainBw = new BufferedWriter(new FileWriter(trainFile))
    val testFile = new File("test.csv")
    val testBw = new BufferedWriter(new FileWriter(testFile))
    //    csvBw.write("id,para from top,label\n")

    PrepareTrainNTestMain(XMLLoaderIgnoringDTD.loadFile(args(0)), args(1), trainBw, testBw, args(2), args(3).toInt)
    trainBw.close()
    testBw.close()
  }
}


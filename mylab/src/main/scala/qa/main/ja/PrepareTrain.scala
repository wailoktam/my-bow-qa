package qa.main.ja

import java.io.{FileWriter, BufferedWriter, File}

import org.apache.lucene.index.{Term, DirectoryReader}
import org.apache.lucene.search.{TermQuery, IndexSearcher}
import org.apache.lucene.store.FSDirectory

import scala.xml._
import org.atilika.kuromoji.Tokenizer
import org.atilika.kuromoji.Token
import com.ibm.icu.text.Normalizer2

object PrepareTrainMain {

  def myNormalize(input:String):String = {
    val tokenizer = Tokenizer.builder.mode(Tokenizer.Mode.NORMAL).build
    val tokens = tokenizer.tokenize(input).toArray
    val normalizer = Normalizer2.getNFCInstance()
    (tokens.map { t => normalizer.normalize(t.asInstanceOf[Token].getSurfaceForm())}).mkString
  }

  def sectPara(id: String):Int =  {
    val paraIDRe = """(?<=\\)\d+""".r
    paraIDRe.findFirstIn(id).getOrElse("0").toInt
  }

  def getParasOfNext(idNum: Int, oldID: String, index_searcher: IndexSearcher):Int = {
    //idNum = 2
    //oldID = 2:450-4-1-1-4
    val newID = oldID + "-" + idNum.toString()
    val term = new Term("id", newID)
    //newID = 2:450-4-1-1-4-2
    System.err.println(s"old id + - num ${oldID+"-"+idNum.toString()}")
    val query = new TermQuery(term)
    if ((index_searcher.search(query, 1).scoreDocs).length == 0) 0
    else {
      val paraSelfCount = for (scoreWDoc <- index_searcher.search(query, 1).scoreDocs) yield {
        index_searcher.doc(scoreWDoc.doc).get("paras")
      }
      val paraCount = paraSelfCount.mkString.toInt + getParasOfDesc(idNum, oldID, index_searcher)
      System.err.println(s"newID in getParasOfNext ${newID}")
      System.err.println(s"paraSelfCount in getParasOfNext ${paraSelfCount.mkString.toInt }")
      System.err.println(s"paraCount in getParasOfNext ${paraCount}")
      paraCount + getParasOfNext(idNum + 1, oldID, index_searcher)
    }
  }

  def getParasOfDesc(idNum: Int, oldID: String, index_searcher: IndexSearcher):Int = {
    //idNum = 4
    //oldID = 2:450-4-1-1

    val newID = oldID + "-" + idNum.toString()
    //newID = 2:450-4-1-1-4
    val term = new Term("id", newID + "-" + "1")
    System.err.println(s"newID plus -1  in getParasOfDesc ${newID +"-"+ "1"}")
    val query = new TermQuery(term)
    if ((index_searcher.search(query, 1).scoreDocs).length == 0) 0
    else {
      val paraSelfCount = for (scoreWDoc <- index_searcher.search(query, 1).scoreDocs) yield {
        index_searcher.doc(scoreWDoc.doc).get("paras")
      }
      val paraCount = paraSelfCount.mkString.toInt + getParasOfNext(2, newID, index_searcher)

      System.err.println(s"paraSelfCount in getParasOfDesc ${paraSelfCount.mkString.toInt }")
      System.err.println(s"paraCount in getParasOfDesc ${paraCount}")
      paraCount + getParasOfDesc(1, newID, index_searcher)
    }
  }


    def getParasOfPrev(idNum: Int, oldID: String, index_searcher: IndexSearcher): Int = {
      //idNum = 4
      //oldID = 2:450-4-1-1
      if (idNum == 0) 0
      else {
        val newID = oldID + "-" + idNum.toString()
        //newID = 2:450-4-1-1-4
        val term = new Term("id", newID)
        System.err.println(s"newID in getParasOfPrev ${newID}")
        val query = new TermQuery(term)
        val paraSelfCount = for (scoreWDoc <- index_searcher.search(query, 1).scoreDocs) yield {
          index_searcher.doc(scoreWDoc.doc).get("paras")
        }
        System.err.println(s"paraSelfCount in getParasOfPrev ${paraSelfCount.mkString}")
        val paraCount = paraSelfCount.mkString.toInt + getParasOfDesc(idNum, oldID, index_searcher)

        System.err.println(s"paraSelfCount in getParasOfPrev ${paraSelfCount.mkString.toInt }")
        System.err.println(s"paraCount in getParasOfPrev ${paraCount}")
        paraCount + getParasOfPrev(idNum - 1, oldID, index_searcher)
      }
    }





    def wholePara(id: String, index_searcher: IndexSearcher): Int = {
      val sectIDRe = """(?<=-)(\d+)(?=[\D]*)$""".r
      val sectIDwSepRe = """-(\d+)(?=[\D]*)$""".r
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
          getParasOfPrev(splitID.toInt-1, newID, index_searcher) + wholePara(newID, index_searcher)
        }
      }
    }



    def checkQuestionAndAnnotation(questionXML: Node, indexDir: FSDirectory, csvBw: BufferedWriter): Unit = {
      val index_reader = DirectoryReader.open(indexDir)
      val index_searcher = new IndexSearcher(index_reader)
      for (doc <- (questionXML \\ "doc")) yield {
        val label = {
          val dtitle = myNormalize((doc \\ "dtitle").text.trim())
          (questionXML \\ "answers") contains { a: Node => dtitle.contains(myNormalize(a.text.trim())) }
        }
        val sectParaNum = sectPara((doc \\ "did").text)
        //doc\\"did".text = 2:450-4-1-1-5/9
        val featLast1 = sectParaNum.toString() :: List(label.toString())
        val paraIDRe = """\/\d+""".r
        val idWoPara = paraIDRe.replaceAllIn((doc \\ "did").text, "")
        //idWoPara = 2:450-4-1-1-5
        val sectIDRe = """(?<=-)(\d+)(?=[\D]*)$""".r
        val sectIDwSepRe = """-(\d+)(?=[\D]*)$""".r
        val splitIDOpt = sectIDRe.findFirstIn(idWoPara.trim())
        //splitIDOpt = Some(5)
        val idWoLastSect = sectIDwSepRe.replaceFirstIn(idWoPara.trim(), "")
        System.err.println(s"idWoParaInCheckQ&A ${idWoPara}")
        System.err.println(s"idWoLastSectInCheckQ&A ${idWoLastSect}")
        System.err.println(s"splitIDinCheckQ&A ${splitIDOpt}")
        //idWoLastSect = 2:450-4-1-1
        val featLast2 = splitIDOpt match {
          case None => {
            sectParaNum.toString() :: featLast1
          }
          case Some(splitID) => {
            if ((splitID.toInt == 0) == false) {
              System.err.println(s"splitIDInCheckQ&A ${splitID}")
//              val temp1 = getParasOfPrev(splitID.toInt - 1, idWoLastSect, index_searcher)
//              val temp2 = wholePara(idWoLastSect, index_searcher)
//              System.err.println(s"temp1InCheckQ&A ${temp1}")
//              System.err.println(s"temp2InCheckQ&A ${temp2}")
//              temp1 + temp2 :: featLast1

              sectParaNum + getParasOfPrev(splitID.toInt - 1, idWoLastSect, index_searcher) + wholePara(idWoLastSect, index_searcher) :: featLast1
            }
            else 0 :: featLast1
          }
        }
        val bwLine = (doc \\ "did").text.trim() :: featLast2
        csvBw.write(bwLine.mkString(",")+"\n")
      }
    }

    def apply(xmlWDocs: Node, indexDir: FSDirectory, csvFile: BufferedWriter) = {
        (xmlWDocs \\ "question") map (checkQuestionAndAnnotation(_, indexDir, csvFile))
      }
    }

object PrepareTrain {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("Usage: scala qa.main.ja.PrepareTrain INPUT_XML INDEX_DIR OUTPUT_CSV")
      System.exit(1)
    }
    val indexDir = FSDirectory.open(new File(args(1)))
    val csvFile = new File(args(2))
    val csvBw = new BufferedWriter(new FileWriter(csvFile))
    csvBw.write("id,para from top,para in section,label\n")
    PrepareTrainMain(XMLLoaderIgnoringDTD.loadFile(args(0)),indexDir, csvBw)
    csvBw.close()
  }
}


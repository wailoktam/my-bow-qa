package qa.main.ja

import java.io.File

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

  def sectPara(id: String):(String,Int) =  {
    val paraIDRe = """\/\d+""".r
    (paraIDRe.replaceAllIn(id, ""),paraIDRe.findFirstIn(id).getOrElse("0").toInt)
  }

  def wholePara(id: String, indexDir: FSDirectory): Int = {
    val sectIDRe = """(-\d+)$""".r
    sectIDRe.findFirstIn(id)
    val index_reader = DirectoryReader.open(indexDir)
    val index_searcher = new IndexSearcher(index_reader)
    val paraIDRe = """\/\d+""".r
    val sectID = paraIDRe.replaceAllIn(id, "")
    val pageID = sectIDRe.replaceAllIn(sectID, "")
    val sectTerm = new Term("id", sectID)
    val pageTerm = new Term("id", pageID)

    val queryWPageID = new TermQuery(pageTerm)
    val queryWSectID = new TermQuery(sectTerm)
    //    val query = parser.parse(sectID)
    //    System.err.println(s"paraid ${id}")
    System.err.println(s"sectid ${sectID}")
    System.err.println(s"pageid ${pageID}")
    System.err.println(s"secttitle ${(for (scoreWDoc <- index_searcher.search(queryWSectID, 1).scoreDocs) yield { index_searcher.doc(scoreWDoc.doc).get("title") }).mkString}")
    //    System.err.println(s"pagetitle ${(for (scoreWDoc <- sectIndex_searcher.search(queryWPageID,1 ).scoreDocs) yield {sectIndex_searcher.doc(scoreWDoc.doc).get("id")}).mkString}")
    (for (scoreWDoc <- index_searcher.search(queryWPageID, 1).scoreDocs) yield { index_searcher.doc(scoreWDoc.doc).get("id") }).mkString.toInt
  }



  def checkQuestionAndAnnotation(questionXML: Node, indexDir: FSDirectory): Unit = {
    for (doc <- (questionXML \\ "doc")) yield {

      val label = {
        val dtitle = myNormalize((doc \\ "dtitle").text.trim())
        (questionXML \\ "answers") contains { a:Node => dtitle.contains(myNormalize(a.text.trim()))}
      }

      val featLast1 = sectPara((doc \\ "did").text)::List(label.toString())
//      val featLast2 = wholePara(featLast1(0),indexDir)::featLast1
//      val featLast3 = sect((doc \\ "did").text)::featLast2
    }



    QuestionAndAnnotation((questionXML \ "@id").text,
      questionXML \\ "text",
      questionXML \\ "meta",
      questionXML \\ "answers",
      questionXML \\ "annotations")
  }

  def formatInXML(newQAndA: QuestionAndAnnotation): Elem = {
    newQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, newAnnotations) =>
        <question>
          { questionText }
          { meta }
          { answers }
          { newAnnotations }
        </question> % Attribute(None, "id", Text(id), Null)
    }
  }

  def apply(xmlWDocs: Node, indexDir: FSDirectory, csvFileName: String) = {
    (xmlWDocs \\ "question") map (checkQuestionAndAnnotation(_,indexDir))
  }
}

object PrepareTrain {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("Usage: scala qa.main.ja.PrepareTrain INPUT_XML WIKIFILE_DIR OUTPUT_CSV")
      System.exit(1)
    }
    val indexDir = FSDirectory.open(new File(args(1)))
    PrepareTrainMain(XMLLoaderIgnoringDTD.loadFile(args(0)),indexDir,args(2))
  }
}


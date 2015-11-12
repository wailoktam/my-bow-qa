package qa.main.ja

import java.io.File
import collection.JavaConverters._
import org.apache.lucene.analysis.ja.JapaneseAnalyzer
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.{ MultiFieldQueryParser, QueryParser }
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.store.{ Directory, FSDirectory }
import org.apache.lucene.search.similarities._

import scala.util.Try

//import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.Set
import scala.xml._
import util.matching._

class SearchDocument(val index_dir: Directory, val maxSearchResults: Int = 100) {
  val analyzer = new JapaneseAnalyzer
  val parser = new QueryParser("text", analyzer)
  //  val parser = new MultiFieldQueryParser(Array("title", "text"), analyzer)
  val index_reader = DirectoryReader.open(index_dir)
  val index_searcher = new IndexSearcher(index_reader)
  // set the similarity function to return tf=1
  //  index_searcher.setSimilarity(new SimilarityWithConstantNOM) // NOM を常に1にする
  index_searcher.setSimilarity(new DefaultSimilarity)
  def extractQuestionAndAnnotation(questionXML: Node): QuestionAndAnnotation = {
    QuestionAndAnnotation((questionXML \ "@id").text,
      questionXML \\ "text",
      questionXML \\ "meta",
      questionXML \\ "answers",
      questionXML \\ "annotations")
  }

  def orderedUnique[A](ls: List[A]) = {
    def loop(set: Set[A], ls: List[A]): List[A] = ls match {
      case hd :: tail if set contains hd => loop(set, tail)
      case hd :: tail                    => hd :: loop(set + hd, tail)
      case Nil                           => Nil
    }
    loop(Set(), ls)
  }

  def annotateWDocs(maxHits: Int, oldQAndA: QuestionAndAnnotation, mustOrShould: Int, boostTopic: java.lang.Float, boostHv: java.lang.Float): QuestionAndAnnotation = {
    oldQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, oldAnnotations) => {

        val origQueryTxt = orderedUnique(((oldAnnotations \\ "clue" \\ "parse") map (_.text)).toList).mkString

        val topic = (oldAnnotations \\ "clue") collectFirst { case n if ((n \\ "dep").length != 0) => (n \\ "dep").text }
        val hv = (oldAnnotations \\ "clue") collectFirst { case n if ((n \\ "dep").length != 0) => (n \\ "head").text }
        var intQueryTxt = origQueryTxt
        Try {
          System.err.println(s"topic ${topic.get.trim}")
          intQueryTxt = origQueryTxt.replaceFirst(topic.get.trim, topic.get.trim + "^" + boostTopic.toString)
        }
        var newQueryTxt = intQueryTxt
        Try {
          System.err.println(s"hv ${hv.get.trim}")
          newQueryTxt = intQueryTxt.replaceAll(hv.get.trim, hv.get.trim + "^" + boostHv.toString)
        }

        var finalQueryTxt = newQueryTxt
        Try {
          System.err.println("hello")
          val digit = """(^10.0)^10.0""".r
          val doubleBoost = """\^\d+\.\d+(\^\d+\.\d+)""".r
          //        val digit(firstBoost) = newQueryTxt
          //        System.err.println(s"firstBoost ${firstBoost}")
          finalQueryTxt = doubleBoost.replaceAllIn(newQueryTxt, "")

        }
        //        val query = parser.parse(finalQueryTxt)
        val query = parser.parse(origQueryTxt)
        System.err.println(s"query ${query.toString}")
        /*
        val bQueries = new org.apache.lucene.search.BooleanQuery()
        val topicQuery = new org.apache.lucene.search.TermQuery(new org.apache.lucene.index.Term("text", topic.getOrElse("")))
        topicQuery.setBoost(boostTopic)
        val hvQuery = new org.apache.lucene.search.TermQuery(new org.apache.lucene.index.Term("text", hv.getOrElse("")))
        hvQuery.setBoost(boostHv)
        bQueries.add(query, org.apache.lucene.search.BooleanClause.Occur.SHOULD)
        bQueries.add(topicQuery, org.apache.lucene.search.BooleanClause.Occur.SHOULD)
        bQueries.add(hvQuery, org.apache.lucene.search.BooleanClause.Occur.SHOULD)
*/

        //        var setTerm: java.util.Set[org.apache.lucene.index.Term] =  scala.collection.mutable.Set[org.apache.lucene.index.Term]().asJava
        //        val rQueries = index_searcher.rewrite(queries)
        //        rQueries.extractTerms(setTerm)
        //        println(setTerm.asScala.head)

        /**
         * old code for ref
         * val searchResult = index_searcher.search(query, maxHits)
         *
         * val docs =
         * for (scoreWDoc <- searchResult.scoreDocs) yield {
         * val doc = index_searcher.doc(scoreWDoc.doc)
         * val id = doc.get("id")
         * val xml = document_map.get(id).get
         * //print(xml)
         * val title = (xml \ "title").text
         * val text = (xml \ "text").text
         * //println(title)
         * //println(index_searcher.explain(query, score_doc.doc))
         * Doc(id, title, text, scoreWDoc.score)
         * }
         *
         * }
         */

        //                  {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "text").text}
        //                  {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "title").text}
        val newAnnotations =
          <annotations>
            {
              for (annotation <- oldAnnotations \\ "annotation") yield { annotation }
            }
            <annotation type="doc" annotator="SearchDocument">
              <docs>
                {
                  for (scoreWDoc <- index_searcher.search(query, maxHits).scoreDocs) yield <doc>
                                                                                             <did>
                                                                                               { index_searcher.doc(scoreWDoc.doc).get("id") }
                                                                                             </did>
                                                                                             <dtitle>
                                                                                               { index_searcher.doc(scoreWDoc.doc).get("title") }
                                                                                             </dtitle>
                                                                                             <dtext>
                                                                                               { index_searcher.doc(scoreWDoc.doc).get("text") }
                                                                                             </dtext>
                                                                                           </doc>
                }
              </docs>
            </annotation>
          </annotations>
        QuestionAndAnnotation(id, questionText, meta, answers, newAnnotations)
      }
      case _ => oldQAndA
    }
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

  def apply(xmlWClues: Node, maxHits: Int, mustOrShould: Int, boostTopic: java.lang.Float, boostHv: java.lang.Float): Seq[Elem] = {
    (xmlWClues \\ "question") map (extractQuestionAndAnnotation) map (annotateWDocs(maxHits, _, mustOrShould, boostTopic, boostHv)) map (formatInXML)
  }
}

object SearchDocument {

  def main(args: Array[String]): Unit = {
    if (args.length < 6) {
      System.err.println("Usage: scala qa.main.ja.SearchDocument INDEX_DIR INPUT_XML OUTPUT_XML MUST1_OR_SHOULD0 BOOST_TOPIC BOOST_HV")
      System.exit(1)
    }
    // create index

    //    val kb_files = recursiveListFiles(new File(args(0))).filter(_.isFile)
    //    val documents_dir = "src/main/resources/ja/Documents"
    //    val index_dir = documents_dir + "/index"
    //    val cdb_file = documents_dir + "/documents.cdb"
    //    new File(index_dir).mkdirs()

    // try search
    val indexDir = FSDirectory.open(new File(args(0)))
    //    val boost:java.util.Map[String,java.lang.Float] = collection.mutable.Map("title"->args(3).toFloat.asInstanceOf[java.lang.Float], "text"->(args(4).toFloat.asInstanceOf[java.lang.Float])).asJava
    val search = new SearchDocument(indexDir)
    val elems = search(XMLLoaderIgnoringDTD.loadFile(args(1)), 10, args(3).toInt, args(4).toFloat.asInstanceOf[java.lang.Float], args(5).toFloat.asInstanceOf[java.lang.Float])
    XML.save(args(2), <questions>
                        {
                          for (elem <- elems) yield {
                            elem
                          }
                        }
                      </questions>, "UTF-8")

  }
}
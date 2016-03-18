package qa.main.ja

import java.io.File
import collection.JavaConverters._
import org.apache.lucene.analysis.ja.JapaneseAnalyzer
import org.apache.lucene.index.{ AtomicReaderContext, IndexReader, Term, DirectoryReader }
import org.apache.lucene.queryparser.classic.{ MultiFieldQueryParser, QueryParser }
import org.apache.lucene.search._
import org.apache.lucene.store.{ Directory, FSDirectory }
import org.apache.lucene.search.similarities._
import org.apache.commons.lang3.StringUtils

import scala.util.Try

//import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.Set
import scala.xml._
import util.matching._

class SearchPage(indexDir: Directory, sentIndexDir: Directory, maxSearchResults: Int = 100) extends SearchDocument(indexDir, maxSearchResults) {

  override def annotateWDocs(maxHits: Int, oldQAndA: QuestionAndAnnotation, mustOrShould: Int, boostTopic: java.lang.Float, boostHv: java.lang.Float): QuestionAndAnnotation = {

    oldQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, oldAnnotations) => {

        val origQueryTxt = orderedUnique(((oldAnnotations \\ "clue" \\ "parse") map (_.text)).toList).mkString

        val topic = (oldAnnotations \\ "clue") collectFirst { case n if ((n \\ "dep").length != 0) => (n \\ "dep").text }
        val hv = (oldAnnotations \\ "clue") collectFirst { case n if ((n \\ "dep").length != 0) => (n \\ "head").text }
        var intQueryTxt = origQueryTxt
        Try {
          //          System.err.println(s"topic ${topic.get.trim}")
          intQueryTxt = origQueryTxt.replaceFirst(topic.get.trim, topic.get.trim + "^" + boostTopic.toString)
        }
        var newQueryTxt = intQueryTxt
        Try {
          //          System.err.println(s"hv ${hv.get.trim}")
          newQueryTxt = intQueryTxt.replaceAll(hv.get.trim, hv.get.trim + "^" + boostHv.toString)
        }

        var finalQueryTxt = newQueryTxt
        Try {
          //          System.err.println("hello")
          val digit = """(^10.0)^10.0""".r
          val doubleBoost = """\^\d+\.\d+(\^\d+\.\d+)""".r
          //        val digit(firstBoost) = newQueryTxt
          //        System.err.println(s"firstBoost ${firstBoost}")
          finalQueryTxt = doubleBoost.replaceAllIn(newQueryTxt, "")

        }
        //        val query = parser.parse(finalQueryTxt)
        val query = parser.parse(origQueryTxt)
        val answerHits = for (answer <- answers \\ "answer") yield {
          val totalHitCountCollector = new TotalHitCountCollector
          indexSearcher.search(parser.parse("\"" + answer.text + "\""), totalHitCountCollector)
          System.err.println(s"answerToPhraseQueriesText ${parser.parse("\"" + answer.text + "\"").toString}")
          totalHitCountCollector.getTotalHits()
        }
        val answerToPhraseQueriesText = for (answer <- answers \\ "answer") yield {
          "\"" + answer.text + "\""
        }

        //        System.err.println(s"query ${query.toString}")
        /*o
        val bQueries = new org.apache.lucene.search.BooleanQuery()
        val topicQuery = new org.apache.lucene.search.TermQuery(new org.apache.lucene.index.Term("text", topic.getOrElse("")))
        topicQuery.setBoost(boostTopic)o
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
              <ansstats>
                {
                  for (answerHit <- answerHits) yield {
                    <answerHit>
                      { answerHit }
                    </answerHit>
                  }
                }
              </ansstats>
              <docs>
                {
                  for (scoreWDoc <- indexSearcher.search(query, maxHits).scoreDocs) yield <doc>
                                                                                            <did>
                                                                                              { indexSearcher.doc(scoreWDoc.doc).get("id") }
                                                                                            </did>
                                                                                            <dtitle>
                                                                                              { indexSearcher.doc(scoreWDoc.doc).get("title") }
                                                                                            </dtitle>
                                                                                            <dtext>
                                                                                              {
                                                                                                val sentNumStream = Stream.iterate(1)(_ + 1).iterator
                                                                                                for (sent <- indexSearcher.doc(scoreWDoc.doc).get("text").split("。")) yield {

                                                                                                  <sent>
                                                                                                    <stext>{ sent }</stext>
                                                                                                    <sscore>{ searchScoreOfSent(sent, query, sentIndexDir) }</sscore>
                                                                                                    <scount>{ sentNumStream.next }</scount>
                                                                                                  </sent>
                                                                                                }
                                                                                              }
                                                                                            </dtext>
                                                                                            <dscore>
                                                                                              { scoreWDoc.score }
                                                                                            </dscore>
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

}

class SearchSect(indexDir: Directory, sentIndexDir: Directory, maxSearchResults: Int = 100) extends SearchDocument(indexDir, maxSearchResults) {

  def searchPageTitleOfSect(id: String): String = {
    val sectIDRe = """(s\d+)+""".r
    val pageID = sectIDRe.replaceAllIn(id, "")
    val pageTerm = new Term("id", pageID)
    val queryWPageID = new TermQuery(pageTerm)
    (for (scoreWDoc <- indexSearcher.search(queryWPageID, 1).scoreDocs) yield {
      indexSearcher.doc(scoreWDoc.doc).get("title")
    }).mkString
  }

  override def annotateWDocs(maxHits: Int, oldQAndA: QuestionAndAnnotation, mustOrShould: Int, boostTopic: java.lang.Float, boostHv: java.lang.Float): QuestionAndAnnotation = {

    oldQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, oldAnnotations) => {

        val origQueryTxt = orderedUnique(((oldAnnotations \\ "clue" \\ "parse") map (_.text)).toList).mkString

        val topic = (oldAnnotations \\ "clue") collectFirst { case n if ((n \\ "dep").length != 0) => (n \\ "dep").text }
        val hv = (oldAnnotations \\ "clue") collectFirst { case n if ((n \\ "dep").length != 0) => (n \\ "head").text }
        var intQueryTxt = origQueryTxt
        Try {
          //          System.err.println(s"topic ${topic.get.trim}")
          intQueryTxt = origQueryTxt.replaceFirst(topic.get.trim, topic.get.trim + "^" + boostTopic.toString)
        }
        var newQueryTxt = intQueryTxt
        Try {
          //          System.err.println(s"hv ${hv.get.trim}")
          newQueryTxt = intQueryTxt.replaceAll(hv.get.trim, hv.get.trim + "^" + boostHv.toString)
        }

        var finalQueryTxt = newQueryTxt
        Try {
          //          System.err.println("hello")
          val digit = """(^10.0)^10.0""".r
          val doubleBoost = """\^\d+\.\d+(\^\d+\.\d+)""".r
          //        val digit(firstBoost) = newQueryTxt
          //        System.err.println(s"firstBoost ${firstBoost}")
          finalQueryTxt = doubleBoost.replaceAllIn(newQueryTxt, "")

        }
        //        val query = parser.parse(finalQueryTxt)
        val query = parser.parse(origQueryTxt)
        val answerHits = for (answer <- answers \\ "answer") yield {
          val totalHitCountCollector = new TotalHitCountCollector
          indexSearcher.search(parser.parse("\"" + answer.text + "\""), totalHitCountCollector)
          System.err.println(s"answerToPhraseQueriesText ${parser.parse("\"" + answer.text + "\"").toString}")
          totalHitCountCollector.getTotalHits()
        }
        val answerToPhraseQueriesText = for (answer <- answers \\ "answer") yield {
          "\"" + answer.text + "\""
        }

        //        System.err.println(s"query ${query.toString}")
        /*o
        val bQueries = new org.apache.lucene.search.BooleanQuery()
        val topicQuery = new org.apache.lucene.search.TermQuery(new org.apache.lucene.index.Term("text", topic.getOrElse("")))
        topicQuery.setBoost(boostTopic)o
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
              <ansstats>
                {
                  for (answerHit <- answerHits) yield {
                    <answerHit>
                      { answerHit }
                    </answerHit>
                  }
                }
              </ansstats>
              <docs>
                {
                  for (scoreWDoc <- indexSearcher.search(query, maxHits).scoreDocs) yield <doc>
                                                                                            <did>
                                                                                              { indexSearcher.doc(scoreWDoc.doc).get("id") }
                                                                                            </did>
                                                                                            <dtitle>
                                                                                              { searchPageTitleOfSect(indexSearcher.doc(scoreWDoc.doc).get("id")) }
                                                                                            </dtitle>
                                                                                            <dtext>
                                                                                              {
                                                                                                val sentNumStream = Stream.iterate(1)(_ + 1).iterator
                                                                                                val noEmptySents = (indexSearcher.doc(scoreWDoc.doc).get("text").split("。")).filter(s => (PrepareTrainNTestMain.myNormalize(s).trim() != ""))
                                                                                                for (noEmptySent <- noEmptySents) yield {
                                                                                                  <sent>
                                                                                                    <stext>{ noEmptySent }</stext>
                                                                                                    <sscore>{ searchScoreOfSent(noEmptySent, query, sentIndexDir) }</sscore>
                                                                                                    <scount>{ sentNumStream.next }</scount>
                                                                                                  </sent>
                                                                                                }
                                                                                              }
                                                                                              s
                                                                                            </dtext>
                                                                                            <dscore>
                                                                                              { scoreWDoc.score }
                                                                                            </dscore>
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

}

class SearchPara(indexDir: Directory, sectIndexDir: Directory, sentIndexDir: Directory, maxSearchResults: Int = 100) extends SearchDocument(indexDir, maxSearchResults) {

  def searchSectTitleOfPara(id: String): String = {

    val sectIndexReader = DirectoryReader.open(sectIndexDir)
    val sectIndexSearcher = new IndexSearcher(sectIndexReader)
    val paraIDRe = """r\d+""".r
    val sectIDRe = """(s\d+)+""".r
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
    System.err.println(s"secttitle ${(for (scoreWDoc <- sectIndexSearcher.search(queryWPageID, 1).scoreDocs) yield { sectIndexSearcher.doc(scoreWDoc.doc).get("title") }).mkString}")
    //    System.err.println(s"pagetitle ${(for (scoreWDoc <- sectIndex_searcher.search(queryWPageID,1 ).scoreDocs) yield {sectIndex_searcher.doc(scoreWDoc.doc).get("id")}).mkString}")
    (for (scoreWDoc <- sectIndexSearcher.search(queryWSectID, 1).scoreDocs) yield { sectIndexSearcher.doc(scoreWDoc.doc).get("title") }).mkString
    //    (for (scoreWDoc <- sectIndex_searcher.search(queryWPageID, 1).scoreDocs) yield { sectIndex_searcher.doc(scoreWDoc.doc).get("title") }).mkString
    //    System.err.println(s"sectTitle ${(for (scoreWDoc <- sectIndex_searcher.search(query,1).scoreDocs) yield {sectIndex_searcher.doc(scoreWDoc.doc).get("title")}).mkString}")
  }

  override def annotateWDocs(maxHits: Int, oldQAndA: QuestionAndAnnotation, mustOrShould: Int, boostTopic: java.lang.Float, boostHv: java.lang.Float): QuestionAndAnnotation = {
    oldQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, oldAnnotations) => {

        val origQueryTxt = orderedUnique(((oldAnnotations \\ "clue" \\ "parse") map (_.text)).toList).mkString

        val topic = (oldAnnotations \\ "clue") collectFirst { case n if ((n \\ "dep").length != 0) => (n \\ "dep").text }
        val hv = (oldAnnotations \\ "clue") collectFirst { case n if ((n \\ "dep").length != 0) => (n \\ "head").text }
        var intQueryTxt = origQueryTxt
        Try {
          //          System.err.println(s"topic ${topic.get.trim}")
          intQueryTxt = origQueryTxt.replaceFirst(topic.get.trim, topic.get.trim + "^" + boostTopic.toString)
        }
        var newQueryTxt = intQueryTxt
        Try {
          //          System.err.println(s"hv ${hv.get.trim}")
          newQueryTxt = intQueryTxt.replaceAll(hv.get.trim, hv.get.trim + "^" + boostHv.toString)
        }

        var finalQueryTxt = newQueryTxt
        Try {
          //          System.err.println("hello")
          val digit = """(^10.0)^10.0""".r
          val doubleBoost = """\^\d+\.\d+(\^\d+\.\d+)""".r
          //        val digit(firstBoost) = newQueryTxt
          //        System.err.println(s"firstBoost ${firstBoost}")
          finalQueryTxt = doubleBoost.replaceAllIn(newQueryTxt, "")

        }
        //        val query = parser.parse(finalQueryTxt)
        val query = parser.parse(origQueryTxt)
        val answerHits = for (answer <- answers \\ "answer") yield {
          val totalHitCountCollector = new TotalHitCountCollector
          indexSearcher.search(parser.parse("\"" + answer.text + "\""), totalHitCountCollector)
          System.err.println(s"answerToPhraseQueriesText ${parser.parse("\"" + answer.text + "\"").toString}")
          totalHitCountCollector.getTotalHits()
        }
        val answerToPhraseQueriesText = for (answer <- answers \\ "answer") yield {
          "\"" + answer.text + "\""
        }
        //        System.err.println(s"query ${query.toString}")
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
         * val sentNumStream = Stream.iterate(1)(_ + 1).iterator
         * val noEmptySents = (indexSearcher.doc(scoreWDoc.doc).get("text").split("。")).filter(s => (PrepareTrainMain.myNormalize(s.trim()) == "") == 0)
         * for (noEmptySent <-  noEmptySents) yield {
         * <check>hello</check>
         * <sent>
         * <stext>{noEmptySent}</stext>
         * <sscore>{searchScoreOfSent(noEmptySent, query, sentIndexDir)}</sscore>
         * <scount>{sentNumStream.next}</scount>
         * </sent>
         */

        //                  {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "text").text}
        //                  {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "title").text}
        val newAnnotations =
          <annotations>
            {
              for (annotation <- oldAnnotations \\ "annotation") yield { annotation }
            }
            <annotation type="doc" annotator="SearchDocument">
              <ansstats>
                {
                  for (answerHit <- answerHits) yield {
                    <answerHit>
                      { answerHit }
                    </answerHit>
                  }
                }
              </ansstats>
              <docs>
                {
                  for (scoreWDoc <- indexSearcher.search(query, maxHits).scoreDocs) yield <doc>
                                                                                            <did>
                                                                                              { indexSearcher.doc(scoreWDoc.doc).get("id") }
                                                                                            </did>
                                                                                            <dtitle>
                                                                                              { searchSectTitleOfPara(indexSearcher.doc(scoreWDoc.doc).get("id")) }
                                                                                            </dtitle>
                                                                                            <dtext>
                                                                                              {
                                                                                                val sentNumStream = Stream.iterate(1)(_ + 1).iterator
                                                                                                val noEmptySents = (indexSearcher.doc(scoreWDoc.doc).get("text").split("。")).filter(s => (PrepareTrainNTestMain.myNormalize(s).trim() != ""))
                                                                                                for (noEmptySent <- noEmptySents) yield {

                                                                                                  <sent>
                                                                                                    <stext>{ noEmptySent }</stext>
                                                                                                    <sscore>{ searchScoreOfSent(noEmptySent, query, sentIndexDir) }</sscore>
                                                                                                    <scount>{ sentNumStream.next }</scount>
                                                                                                  </sent>
                                                                                                }

                                                                                              }
                                                                                            </dtext>
                                                                                            <dscore>
                                                                                              { scoreWDoc.score }
                                                                                            </dscore>
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
}

abstract class SearchDocument(val indexDir: Directory, val maxSearchResults: Int = 100) {
  val analyzer = new JapaneseAnalyzer
  val parser = new QueryParser("text", analyzer)
  //  val parser = new MultiFieldQueryParser(Array("text","title","firstpara"), analyzer)
  val indexReader = DirectoryReader.open(indexDir)
  val indexSearcher = new IndexSearcher(indexReader)

  // set the similarity function to return tf=1
  //  index_searcher.setSimilarity(new SimilarityWithConstantNOM) // NOM を常に1にする
  indexSearcher.setSimilarity(new DefaultSimilarity)
  def extractQuestionAndAnnotation(questionXML: Node): QuestionAndAnnotation = {
    QuestionAndAnnotation((questionXML \ "@id").text,
      questionXML \\ "text",
      questionXML \\ "meta",
      questionXML \\ "answers",
      questionXML \\ "annotations")
  }

  def powerTrim(text: String): String = {
    val fullSpaceRe = """　""".r
    fullSpaceRe.replaceAllIn(text.trim(), "")
  }

  def searchScoreOfSent(sentText: String, realQuery: Query, sentIndexDir: Directory): Float = {
    val sentIndexReader = DirectoryReader.open(sentIndexDir)
    val sentIndexSearcher = new IndexSearcher(sentIndexReader)
    val escapedText = PrepareTrainNTestMain.myNormalize(org.apache.lucene.queryparser.classic.QueryParserBase.escape(powerTrim(sentText)))
    if (StringUtils.isEmpty((escapedText)) == false) {
      val toBeFilterQuery = parser.parse(escapedText)
      val filter = new QueryWrapperFilter(toBeFilterQuery)
      if (sentIndexSearcher.search(realQuery, filter, 1).scoreDocs.length != 0) sentIndexSearcher.search(realQuery, filter, 1).scoreDocs(0).score
      else 0
    } else 0
  }

  def orderedUnique[A](ls: List[A]) = {
    def loop(set: Set[A], ls: List[A]): List[A] = ls match {
      case hd :: tail if set contains hd => loop(set, tail)
      case hd :: tail                    => hd :: loop(set + hd, tail)
      case Nil                           => Nil
    }
    loop(Set(), ls)
  }

  def annotateWDocs(maxHits: Int,
                    oldQAndA: QuestionAndAnnotation,
                    mustOrShould: Int,
                    boostTopic: java.lang.Float, boostHv: java.lang.Float): QuestionAndAnnotation

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
    (xmlWClues \\ "question")
      .map(extractQuestionAndAnnotation)
      .map(annotateWDocs(maxHits, _, mustOrShould, boostTopic, boostHv))
      .map(formatInXML)
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
    val tableRe = """(?<=\{\{)[^\{\}]+(?=\}\})""".r
    val pageIndexRe = """(?<=.*)page(?=.*)""".r
    val sectIndexRe = """(?<=.*)sect(?=.*)""".r
    val paraIndexRe = """(?<=.*)para(?=.*)""".r
    val otherThanSentIndexRe = """(?<=.*)page|sect|para(?=.*)""".r
    val sectIndexName = paraIndexRe.replaceAllIn(args(0), "sect")
    val sentIndexName = otherThanSentIndexRe.replaceAllIn(args(0), "sent")
    System.err.println(s"sentenceIndexDir: ${sentIndexName}")
    System.err.println(s"sectionIndexDir: ${sectIndexName}")
    val sectIndexDir = FSDirectory.open(new File(sectIndexName))
    val sentIndexDir = FSDirectory.open(new File(sentIndexName))
    val search = pageIndexRe.findFirstIn(args(0)) match {
      case Some(m) => new SearchPage(indexDir, sentIndexDir)
      case None => sectIndexRe.findFirstIn(args(0)) match {
        case Some(m) => new SearchSect(indexDir, sentIndexDir)
        case None    => new SearchPara(indexDir, sectIndexDir, sentIndexDir)
      }
    }

    //    val boost:java.util.Map[String,java.lang.Float] = collection.mutable.Map("title"->args(3).toFloat.asInstanceOf[java.lang.Float], "text"->(args(4).toFloat.asInstanceOf[java.lang.Float])).asJava

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
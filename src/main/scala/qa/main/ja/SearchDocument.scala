import java.io.File

import org.apache.lucene.analysis.ja.JapaneseAnalyzer
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.store.{FSDirectory, Directory}
import qa.main.ja.{SimilarityWithConstantTF, Indexing, XMLLoaderIgnoringDTD, QuestionAndAnnotation}

import scala.xml._

class SearchDocument(val index_dir: Directory, val maxSearchResults: Int = 5) {
  val analyzer = new JapaneseAnalyzer
  val parser = new QueryParser("text", analyzer)
  //val parser = new MultiFieldQueryParser(Array("title", "text"), analyzer)
  val index_reader = DirectoryReader.open(index_dir)
  val index_searcher = new IndexSearcher(index_reader)
  // set the similarity function to return tf=1
  index_searcher.setSimilarity(new SimilarityWithConstantTF) // TF を常に1にする

  def extractQuestionAndAnnotation(questionXML: Node):QuestionAndAnnotation = {
    QuestionAndAnnotation((questionXML \ "@id").text,
      questionXML \\ "text",
      questionXML \\ "meta",
      questionXML \\ "answers",
      questionXML \\ "annotations")
  }

  def orderedUnique[A](ls: List[A]) = {
    def loop(set: Set[A], ls: List[A]): List[A] = ls match {
      case hd :: tail if set contains hd => loop(set, tail)
      case hd :: tail => hd :: loop(set + hd, tail)
      case Nil => Nil
    }
    loop(Set(), ls)
  }



  def annotateWDocs (maxHits: Int, oldQAndA: QuestionAndAnnotation): QuestionAndAnnotation ={
    oldQAndA match {
      case QuestionAndAnnotation(id,questionText,meta,answers,oldAnnotations) => {
        val query = parser.parse(orderedUnique(((oldAnnotations \\ "clue" \\ "parse") map (_.text)).toList).mkString)
        //println(query)
        /**
        old code for ref
        val searchResult = index_searcher.search(query, maxHits)

        val docs =
          for (scoreWDoc <- searchResult.scoreDocs) yield {
            val doc = index_searcher.doc(scoreWDoc.doc)
            val id = doc.get("id")
            val xml = document_map.get(id).get
            //print(xml)
            val title = (xml \ "title").text
            val text = (xml \ "text").text
            //println(title)
            //println(index_searcher.explain(query, score_doc.doc))
            Doc(id, title, text, scoreWDoc.score)
          }

        }
          */

        //                  {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "text").text}
        //                  {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "title").text}
        val newAnnotations =
          <annotations>
            {for (annotation <- oldAnnotations \\ "annotation") yield
          {annotation}}
            <annotation type="doc" annotator="SearchDocument">
              <docs>
                {for (scoreWDoc <- index_searcher.search(query, maxHits).scoreDocs) yield
              <doc>
                <did>
                  {index_searcher.doc(scoreWDoc.doc).get("id")}
                </did>
                <dtitle>

                  {index_searcher.doc(scoreWDoc.doc).get("title")}

                </dtitle>
                <dtext>
                  {index_searcher.doc(scoreWDoc.doc).get("text")}

                </dtext>
              </doc>}
              </docs>
            </annotation>
          </annotations>
        QuestionAndAnnotation(id,questionText,meta,answers,newAnnotations)
      }
      case _ => oldQAndA
    }
  }

  def formatInXML(newQAndA: QuestionAndAnnotation):Elem ={
    newQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, newAnnotations) =>
        <question>
          {questionText}
          {meta}
          {answers}
          {newAnnotations}
        </question> % Attribute (None, "id", Text(id), Null)
    }
  }


  def apply(xmlWClues: Node, maxHits: Int): Seq[Elem] = {
    (xmlWClues \\ "question") map (extractQuestionAndAnnotation) map (annotateWDocs(maxHits,_)) map (formatInXML)
  }
}

object SearchDocument {


  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      System.err.println("Usage: scala qa.main.ja.SearchDocument INDEX_DIR INPUT_XML OUTPUT_XML")
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
    val search = new SearchDocument(indexDir)
    val elems = search(XMLLoaderIgnoringDTD.loadFile(args(1)),10)
    XML.save(args(2), <questions>
      {for (elem <- elems) yield {
        elem
      }}
    </questions>, "UTF-8")

  }
}
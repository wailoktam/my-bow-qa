package qa.main.ja

import java.io.File
import java.io.StringReader
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.ja.JapaneseAnalyzer
import org.apache.lucene.document._
import org.apache.lucene.document._
import org.apache.lucene.document.Field.Store
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.index._
import org.apache.lucene.store._
import org.apache.lucene.search._
import org.apache.lucene.search.similarities._
import org.apache.lucene.util.Version
import scala.xml.{ XML, Elem }
import scala.xml.factory.XMLLoader

import scala.xml.{Document=>_,_}

/**
 * 検索結果文書のクラス
 * @param id 文書ID（文書XMLの <id>）
 * @param title 文書のタイトル
 * @param text 文書の本文
 * @param score 検索スコア
 */
case class Doc(id: String, title: String, text: String, score: Double)


/**
 * tf を常に1にするクラス
 * lucene で使われる
 */
class SimilarityWithConstantTF extends DefaultSimilarity {
  override def tf(freq: Float): Float = 1
  //override def lengthNorm(state: FieldInvertState): Float = 1
}

/**
 * lucene で文書検索を行うクラス
 * @param index_dir インデックスを置くディレクトリ
 * @param document_map 文書集合
 */
class SearchDocument(val index_dir: Directory, val document_map: Map[String, Elem], val maxSearchResults: Int = 5) {
  val analyzer = Indexing.makeAnalyzer()
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
                {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "title").text}
              </dtitle>
              <dtext>
                {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "text").text}
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



object Indexing {
  /**
   * lucene の Analyzer を作る
   * @return
   */
  def makeAnalyzer(): Analyzer = {
    new JapaneseAnalyzer
  }

  /**
   * 特殊文字などを取り除いて、文書をきれいにする
   * @param str
   * @return
   */
  def normalize(str: String): String = {
    val lines = str.split("\n+")
    val regex_old_character = """.*([･…]).*""".r // ignore lines including an old character (maybe a citation from old document)
    val regex_page_number = """(p\..*)""".r // ignore lines of page number
    val normalized_lines =
      for (line <- lines) yield {
        line.trim().replaceAll("""^　*""", "").replaceAll("""　*$""", "") match {
          case regex_old_character(_) => ""
          case regex_page_number(_)   => ""
          case s =>
            s.replaceAll("""^注""", "").
              replaceAll("""\s*[(（].*?[)）]$""", ""). // remove ending parenthesis
              replaceAll("""^[〔（［〈].+[〉］）〕]\s*""", ""). // remove section number, <コラム>, etc.
              replaceAll("""^\d+[\.　]\s*""", ""). // remove section number
              replaceAll("""[●■①-⑳➀-➉➊-➓]""", ""). // remove special characters
              replace("　", "")
        }
      }
    normalized_lines.filter(_.nonEmpty).mkString("\n")
  }

  /*
  def makeIndexOnFile(target_file_names: Array[String], index_dir_name: String, document_cdb_name: String): Directory = {
    val index_dir = FSDirectory.open(new File(index_dir_name))
    val documents = makeIndexMain(target_file_names, index_dir)
    val cdb_make = new CDBMapMake()
    cdb_make.open(document_cdb_name)
    documents foreach { d: (String, Elem) => cdb_make.add(d._1, d._2.toString) }
    cdb_make.close()
    index_dir
  }
*/

  /**
   * Create index on memory from RITE-VAL style documents
   * @param target_file_names
   * @return
   */
  def makeIndexOnMemory(target_file_names: List[File]): (Directory, Map[String, Elem]) = {
    val index_dir = new RAMDirectory
    val documents = makeIndexMain(target_file_names, index_dir)
    (index_dir, documents.toMap)
  }


  private def makeIndexMain(knowledgeFiles: List[File], indexDir: Directory): Array[(String, Elem)] = {
//    val debugList = List("/home/wailoktam/qa/input/knowledge/rite2-ja-textbook.xml","/home/wailoktam/qa/input/knowledge/riteval-ja-textbook2.xml", "/home/wailoktam/qa/input/knowledge/wiki_00")
    val analyzer = makeAnalyzer()
    val config = new IndexWriterConfig(Version.LUCENE_4_10_0, analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    config.setSimilarity(new SimilarityWithConstantTF)
    val writer = new IndexWriter(indexDir, config) // overwrite existing index
    var id = 0
    val documents: Array[(String, Elem)] =
//      (for (target_file_name <- target_file_names) yield {
      (for (knowledgeFile <- knowledgeFiles) yield {

        System.err.println(s"kb path: ${knowledgeFile.getAbsolutePath()}")
        val target_xml = XML.loadFile(knowledgeFile.getAbsolutePath())
        // each <p> as a doc
        /*
        (for (p <- target_xml \\ "p") yield {
          //if (p.text != normalize(p.text)) println(p.text + "\n\t->" + normalize(p.text))
          val text = normalize(p.text)
          if (text.nonEmpty) {
            val idstr = id.toString
            val document = new Document()
            document.add(new StringField("id", idstr, Store.YES))
            document.add(new TextField("text", new StringReader(text)))
            writer.addDocument(document)
            val xml_doc = <page><title></title><text>{ text }</text></page>
            //println(xml_doc)
            id += 1
            Some(idstr -> xml_doc)
          } else
            None
        }).flatten
        */
        // each <page> as a document

        for (page <- target_xml \\ "page") yield {
          val id = (page \ "id").text
          val title = (page \ "title").text
          val text = (page \ "text").text
          //println(title)
          val document = new Document()
          document.add(new StringField("id", id, Store.YES))
          document.add(new TextField("text", new StringReader(title + text)))
          //document.add(new TextField("text", new StringReader(text)))
          writer.addDocument(document)
          val xml_doc = <page><title>{ title }</title><text>{ text }</text></page>
          //println(xml_doc)
          id -> xml_doc
        }

      }).flatten.toArray
    writer.close()
    documents
  }
}



/**
 * 文書検索を試すプログラム
 */
object SearchDocument {

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      System.err.println("Usage: scala qa.main.ja.SearchDocument KB_PATH INPUT_XML OUTPUT_XML")
      System.exit(1)
    }
    // create index
    val kb_files = getListOfFiles(args(0))
    //    val documents_dir = "src/main/resources/ja/Documents"
    //    val index_dir = documents_dir + "/index"
    //    val cdb_file = documents_dir + "/documents.cdb"
    //    new File(index_dir).mkdirs()
    println("Creating index")
    //SearchDocument.makeIndexOnFile(target_files, index_dir, cdb_file)
    val (index, documents) = Indexing.makeIndexOnMemory(kb_files)
    println("done")
    // try search
    val search = new SearchDocument(index, documents)
    val elems = search(XMLLoaderIgnoringDTD.loadFile(args(1)),10)
    XML.save(args(2), <questions>
      {for (elem <- elems) yield {
        elem
      }}
    </questions>, "UTF-8")
  }
}


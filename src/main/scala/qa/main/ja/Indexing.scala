package qa.main.ja

import java.io.File
import java.io.StringReader
import org.apache.commons.lang3.StringEscapeUtils
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
import scala.io.Source
import scala.xml.pull._
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer

import scala.xml.{Document=>_,_}

/**
 * 検索結果文書のクラス
 * @param id 文書ID（文書XMLの <id>）
 * @param title 文書のタイトル
 * @param text 文書の本文
 * @param score 検索スコア
 */
case class Doc(id: String, title: String, text: String, score: Double)

object pullAndAdd {
/**
  def attrsToString(attrs: MetaData) = {
    attrs.length match {
      case 0 => ""
      case _ => attrs.map((m: MetaData) => " " + m.key + "='" + StringEscapeUtils.escapeXml11(m.value.toString) + "'").reduceLeft(_ + _)
    }
  }
*/

def attrsToString(attrs: MetaData) = {
  attrs.length match {
    case 0 => ""
    case _ => attrs.map((m: MetaData) => " " + m.key + "='" + StringEscapeUtils.escapeXml11(m.value.toString) + "'").reduceLeft(_ + _)
  }
}

  def addXMLToDoc(writer: IndexWriter, buf: ArrayBuffer[String]):(String,Elem) = {
    System.err.println("buf",buf.mkString)
    val x = XML.loadString(buf.mkString)
//    val x = buf.mkString
    val id = (x \ "@id").text
    //            System.err.println(s"id: ${id}")
    val title = (x \ "@title").text
    val text = x.text
    //            System.err.println(s"text: ${text}")
    val document = new Document()
    document.add(new StringField("id", id, Store.YES))
    document.add(new TextField("text", new StringReader(text)))
    document.add(new TextField("title", new StringReader(title)))
    writer.addDocument(document)
    val xml_doc = <page><title>{ title }</title><text>{ text }</text></page>
    id -> xml_doc
//    val xml_doc = <page><x>{ x }</x></page>
//    x -> xml_doc
  }

  def apply(writer: IndexWriter, xmlFile: String): Array[(String, Elem)] = {
    val xml = new XMLEventReader(Source.fromFile(xmlFile))
    var insideDoc = false
    var buf = ArrayBuffer[String]()
    System.err.println(s"buf length: ${buf.length}")
    var idXmlPairs = ArrayBuffer[(String,Elem)]()

    for (event <- xml) {
      event match {
        case EvElemStart(_, "doc", attrs, _) => {
          insideDoc = true
          buf += "<" + "doc" + attrsToString(attrs) + ">"
        }
        case EvElemEnd(_, "doc") => {
          buf += "</doc>"
          insideDoc = false
          idXmlPairs += addXMLToDoc(writer,buf)
          buf.clear
        }
        case EvText(t) => {
          if (insideDoc) {
            buf += (t)
          }
        }
        case _ => // ignore
      }
    }
    idXmlPairs.toArray
  }
}


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
                // id mapped to xml. necessary? does each doc comes with 1 id, 1 text and 1 title?
                // can or cannot do {index_searcher.doc(scoreWDoc.doc).get("title")}
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


  def cleanFile(fileName:String):Array[String] = {
    val tagRe = """<\/?doc.*?>""".r
    val bufferedSource = Source.fromFile(fileName)
    val lines = bufferedSource.getLines.toArray
//    System.err.println(s"linesb4: ${lines.mkString}")
//    System.err.println(s"linesLength: ${Source.fromFile(fileName).getLines.length}")
//    System.err.println(s"linesb4: ${lines}")
    val outLines = new Array[String](lines.length)
//    for (line <- lines) yield {
    for ((line,lineNo) <- lines.zipWithIndex) yield {
//      System.err.println(s"lineb4: ${line}")
      if (tagRe.findFirstIn(line)!=None)
        {
//          System.err.println(s"hit line: ${line}")
          outLines(lineNo) = line
        }
      else
        {
//          System.err.println(s"miss line: ${line}")
          outLines(lineNo) = StringEscapeUtils.escapeXml11(line)
        }
    }
    bufferedSource.close
    outLines
  }




//  private def makeIndexMain(knowledgeFiles: Array[String], indexDir: Directory): Array[(String, Elem)] = {
  private def makeIndexMain(knowledgeFiles: Array[String], indexDir: Directory): Unit = {

//    val debugList = List("/home/wailoktam/qa/input/knowledge/rite2-ja-textbook.xml","/home/wailoktam/qa/input/knowledge/riteval-ja-textbook2.xml", "/home/wailoktam/qa/input/knowledge/wiki_00")
    val analyzer = makeAnalyzer()
    val config = new IndexWriterConfig(Version.LUCENE_4_10_0, analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    config.setSimilarity(new SimilarityWithConstantTF)
    val writer = new IndexWriter(indexDir, config) // overwrite existing index
    var id = 0
/**
    for (knowledgeFile <- knowledgeFiles) yield {
      System.err.println(s"preprocessing file: ${knowledgeFile}")
      val contentToWrite = "<file>\n" + cleanFile(knowledgeFile).mkString + "</file>"
      val bw = new java.io.BufferedWriter(new java.io.FileWriter(new File(knowledgeFile)))
      bw.write(contentToWrite)
      bw.close()
    }
*/
    knowledgeFiles.map(pullAndAdd(writer,_)).flatten.toArray
//  val docXmlPairs = knowledgeFiles.map(pullAndAdd(writer,_)).flatten.toArray
    writer.close
//    docXmlPairs

/**
      (for (knowledgeFile <- knowledgeFiles) yield {
          for (doc <- XML.loadFile(knowledgeFile) \\ "doc") yield {
            val id = (doc \ "@id").text
//            System.err.println(s"id: ${id}")
            val title = (doc \ "@title").text
//            System.err.println(s"title: ${title}")
            val text = doc.text
//            System.err.println(s"text: ${text}")
            val document = new Document()
            document.add(new StringField("id", id, Store.YES))
            document.add(new TextField("text", new StringReader(title + text)))
            writer.addDocument(document)
            val xml_doc = <page><title>{ title }</title><text>{ text }</text></page>
            id -> xml_doc
          }
      }).flatten.toArray
    writer.close()
**/
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }


  def makeFileList(dirName:String):Array[String]={
    recursiveListFiles(new File(dirName)).filter(_.isFile).map(_.getAbsolutePath)
  }

/**
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


  def apply(knowDirName:String, indexDirName: String):(Directory, Map[String, Elem]) = {
    val indexDir = FSDirectory.open(new File(indexDirName))
    val documents = makeIndexMain(makeFileList(knowDirName), indexDir)
//    (indexDir, documents.toMap)
  }
}



object SearchDocument {


  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      System.err.println("Usage: scala qa.main.ja.SearchDocument KB_DIR INDEX_DIR INPUT_XML OUTPUT_XML")
      System.exit(1)
    }
    // create index



//    val kb_files = recursiveListFiles(new File(args(0))).filter(_.isFile)
    //    val documents_dir = "src/main/resources/ja/Documents"
    //    val index_dir = documents_dir + "/index"
    //    val cdb_file = documents_dir + "/documents.cdb"
    //    new File(index_dir).mkdirs()
    println("Creating index")
    //SearchDocument.makeIndexOnFile(target_files, index_dir, cdb_file)
    //documents necessary?
    val (index, documents) = Indexing(args(0),args(1))
    println("done")
    // try search

    val search = new SearchDocument(index, documents)
    val elems = search(XMLLoaderIgnoringDTD.loadFile(args(2)),10)
    XML.save(args(3), <questions>
      {for (elem <- elems) yield {
        elem
      }}
    </questions>, "UTF-8")

  }
}


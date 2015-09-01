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


//case class Doc(id: String, title: String, text: String, score: Double)

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

  def addXMLToDoc(writer: IndexWriter, buf: ArrayBuffer[String]):Unit = {
//  def addXMLToDoc(writer: IndexWriter, buf: ArrayBuffer[String]):(String,Elem) = {
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
    document.add(new TextField("text", text, Store.YES))
    document.add(new TextField("title", title, Store.YES))
    writer.addDocument(document)
//    val xml_doc = <page><title>{ title }</title><text>{ text }</text></page>
//    id -> xml_doc
//    val xml_doc = <page><x>{ x }</x></page>
//    x -> xml_doc
  }

  def apply(writer: IndexWriter, xmlFile: String): Unit = {
//  def apply(writer: IndexWriter, xmlFile: String): Array[(String, Elem)] = {
    val xml = new XMLEventReader(Source.fromFile(xmlFile))
    var insideDoc = false
    var buf = ArrayBuffer[String]()
    System.err.println(s"buf length: ${buf.length}")
//    var idXmlPairs = ArrayBuffer[(String,Elem)]()

    for (event <- xml) {
      event match {
        case EvElemStart(_, "doc", attrs, _) => {
          insideDoc = true
          buf += "<" + "doc" + attrsToString(attrs) + ">"
        }
        case EvElemEnd(_, "doc") => {
          buf += "</doc>"
          insideDoc = false
//          idXmlPairs += addXMLToDoc(writer,buf)
          addXMLToDoc(writer,buf)
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
//    idXmlPairs.toArray
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

class Indexing {


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
    val analyzer = new JapaneseAnalyzer()
    val config = new IndexWriterConfig(Version.LUCENE_4_10_0, analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    config.setSimilarity(new SimilarityWithConstantTF)
    val writer = new IndexWriter(indexDir, config) // overwrite existing index
//    var id = 0
/**
    for (knowledgeFile <- knowledgeFiles) yield {
      System.err.println(s"preprocessing file: ${knowledgeFile}")
      val contentToWrite = "<file>\n" + cleanFile(knowledgeFile).mkString + "</file>"
      val bw = new java.io.BufferedWriter(new java.io.FileWriter(new File(knowledgeFile)))
      bw.write(contentToWrite)
      bw.close()
    }
*/
    knowledgeFiles.map(pullAndAdd(writer,_))
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


  def apply(knowDirName:String, indexDirName: String):Unit = {
    val indexDir = FSDirectory.open(new File(indexDirName))
    val documents = makeIndexMain(makeFileList(knowDirName), indexDir)
//    (indexDir, documents.toMap)
  }
}



object Indexing  {


  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("Usage: scala qa.main.ja.Indexing KB_DIR INDEX_DIR")
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
    val createIndex = new Indexing
    createIndex(args(0),args(1))
    println("done")
    // try search


  }
}


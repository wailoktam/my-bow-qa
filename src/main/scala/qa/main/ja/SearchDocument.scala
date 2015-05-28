/**
 * lucene を使って文書検索を行う簡単なプログラム
 * RITE-VAL document 形式の文書を仮定
 * <page>
 *   <id>...</id>
 *   <title>...</title>
 *   <text>...</text>
 * </page>
 */

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

/**
 * 検索結果文書のクラス
 * @param id 文書ID（文書XMLの <id>）
 * @param title 文書のタイトル
 * @param text 文書の本文
 * @param score 検索スコア
 */
case class SearchResult(id: String, title: String, text: String, score: Double)

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
class SearchDocument(val index_dir: Directory, val document_map: Map[String, Elem]) {
  val analyzer = SearchDocument.makeAnalyzer()
  val parser = new QueryParser("text", analyzer)
  //val parser = new MultiFieldQueryParser(Array("title", "text"), analyzer)
  val index_reader = DirectoryReader.open(index_dir)
  val index_searcher = new IndexSearcher(index_reader)
  // set the similarity function to return tf=1
  index_searcher.setSimilarity(new SimilarityWithConstantTF) // TF を常に1にする

  /**
   * 文書を検索する
   * @param input クエリ
   * @param k 検索結果の数
   * @return 検索結果
   */
  def apply(input: String, k: Int = 1): Array[SearchResult] = {
    val query = parser.parse(input)
    //println(query)
    val results = index_searcher.search(query, k)
    val documents =
      (for (score_doc <- results.scoreDocs) yield {
        val doc = index_searcher.doc(score_doc.doc)
        val id = doc.get("id")
        val xml = document_map.get(id).get
        //print(xml)
        val title = (xml \ "title").text
        val text = (xml \ "text").text
        //println(title)
        //println(index_searcher.explain(query, score_doc.doc))
        SearchResult(id, title, text, score_doc.score)
      }).toArray
    documents
  }
}

/**
 * SearchDocument クラスで使うもろもろの処理をまとめたオブジェクト
 */
object SearchDocument {
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

  /**
   * Create index files from RITE-VAL style documents
   * Assuming the following structure
   * <page><title>...</title><id>...</id><text>...</text>
   * @param target_file_names
   * @param index_dir_name
   * @param document_cdb_name
   * @return
   */
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
  def makeIndexOnMemory(target_file_names: Array[String]): (Directory, Map[String, Elem]) = {
    val index_dir = new RAMDirectory
    val documents = makeIndexMain(target_file_names, index_dir)
    (index_dir, documents.toMap)
  }

  /**
   * インデックスを作るメインプログラム
   * @param target_file_names
   * @param index_dir
   * @return
   */
  private def makeIndexMain(target_file_names: Array[String], index_dir: Directory): Array[(String, Elem)] = {
    val analyzer = makeAnalyzer()
    val config = new IndexWriterConfig(Version.LUCENE_4_10_0, analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    config.setSimilarity(new SimilarityWithConstantTF)
    val writer = new IndexWriter(index_dir, config) // overwrite existing index
    var id = 0
    val documents: Array[(String, Elem)] =
      (for (target_file_name <- target_file_names) yield {
        val target_xml = XML.loadFile(target_file_name)
        // each <p> as a document
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
        // each <page> as a document
        /*
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
        */
      }).flatten.toArray
    writer.close()
    documents
  }
}

/**
 * 文書検索を試すプログラム
 */
object SearchTest {
  def main(args: Array[String]): Unit = {
    // create index
    val target_files = Array("input/RITEVAL_JA_training/FV/rite2-ja-textbook.xml",
      "input/RITEVAL_JA_training/FV/riteval-ja-textbook2.xml")
    //    val documents_dir = "src/main/resources/ja/Documents"
    //    val index_dir = documents_dir + "/index"
    //    val cdb_file = documents_dir + "/documents.cdb"
    //    new File(index_dir).mkdirs()
    println("Creating index")
    //SearchDocument.makeIndexOnFile(target_files, index_dir, cdb_file)
    val (index, documents) = SearchDocument.makeIndexOnMemory(target_files)
    println("done")
    // try search
    val search = new SearchDocument(index, documents)
    //val results = search("アメリカ 黒船", 10)
    val results = search("大久保利通を中心とする政権のもとでは，内閣総理大臣を首班として政治が運営されていた。", 10)
    for (result <- results) {
      println("%s %s: %f".format(result.id, result.title, result.score))
    }
  }
}

package qa.main.ja

import java.io.{ FileWriter, BufferedWriter, File }
import java.util.regex._
import org.apache.commons.lang3.StringEscapeUtils
import org.apache.lucene.analysis.ja.JapaneseAnalyzer
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.search.similarities._
import org.apache.lucene.util.Version
import org.apache.lucene.store._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.xml.pull._
import scala.xml.{ Document => _, XML, _ }

//case class Doc(id: String, title: String, text: String, score: Double)

/**
 *
 * class PullAndAdd(writer: IndexWriter) {
 * /**
 * def attrsToString(attrs: MetaData) = {
 * attrs.length match {
 * case 0 => ""
 * case _ => attrs.map((m: MetaData) => " " + m.key + "='" + StringEscapeUtils.escapeXml11(m.value.toString) + "'").reduceLeft(_ + _)
 * }
 * }
 * */
 *
 * def attrsToString(attrs: MetaData) = {
 * val nonDocTagRe = """<(?!\/?doc(?=>|\s.*>))\/?.*?>""".r
 * attrs.length match {
 * case 0 => ""
 * case _ => attrs.map((m: MetaData) => " " + m.key + "='" + StringEscapeUtils.escapeXml11(nonDocTagRe.replaceAllIn(m.value.toString, "")) + "'").reduceLeft(_ + _)
 * }
 * }
 *
 * def addXMLToDoc(writer: IndexWriter, buf: ArrayBuffer[String]): Unit = {
 * //  def addXMLToDoc(writer: IndexWriter, buf: ArrayBuffer[String]):(String,Elem) = {
 * //    System.err.println("buf", buf.mkString)
 *
 * }
 *
 *
 *
 *
 *
 * def apply(writer: IndexWriter, xmlFile: String): Unit = {
 * //  def apply(writer: IndexWriter, xmlFile: String): Array[(String, Elem)] = {
 * val xml = new XMLEventReader(Source.fromFile(xmlFile))
 * var insideDoc = false
 * var buf = ArrayBuffer[String]()
 *
 * //    var idXmlPairs = ArrayBuffer[(String,Elem)]()
 *
 * for (event <- xml) {
 * event match {
 * case EvElemStart(_, "doc", attrs, _) => {
 * insideDoc = true
 * buf += "<" + "doc" + attrsToString(attrs) + ">"
 * }
 * case EvElemEnd(_, "doc") => {
 * //          System.err.println(s"buf in event parser: ${buf}")
 * buf += "</doc>"
 * insideDoc = false
 * //          idXmlPairs += addXMLToDoc(writer,buf)
 * addXMLToDoc(writer, buf)
 * buf.clear
 * }
 * case EvText(t) => {
 * if (insideDoc) {
 * buf += t
 * }
 * }
 * case _ => // ignore
 * }
 * }
 * //    idXmlPairs.toArray
 * }
 * }
 *
 *
 *
 * class PullAndAddPage (writer: IndexWriter) extends PullAndAdd(writer) {
 *
 * override def addXMLToDoc(writer: IndexWriter, buf: ArrayBuffer[String]): Unit ={
 * val x = XML.loadString(buf.mkString)
 * //    val x = buf.mkString
 * val id = (x \ "@id").text
 * //            System.err.println(s"id: ${id}")
 * val title = (x \ "@title").text
 * //    val firstpara = (x \ "@firstpara").text
 * val text = x.text
 *
 * val wholeDocument = new Document()
 * wholeDocument.add(new StringField("id", id, Store.YES))
 * wholeDocument.add(new TextField("text", text, Store.YES))
 * wholeDocument.add(new TextField("title", title, Store.YES))
 * writer.addDocument(wholeDocument)
 * //    System.err.println(s"text: ${text}")
 * for ((p,paraCounter)<- paras.zipWithIndex) {
 * System.err.println(s"paracounter: ${paraCounter}")
 * if ((p == "") == false) {
 * if (paraCounter == 1) {
 * val wholeDocument = new Document()
 * System.err.println(s"pid: ${id}")
 * System.err.println(s"pid: ${text}")
 * wholeDocument.add(new StringField("id", id, Store.YES))
 * wholeDocument.add(new TextField("text", text, Store.YES))
 * wholeDocument.add(new IntField("isDoc", 1, Store.YES))
 * writer.addDocument(wholeDocument)
 * val titleAsDocument = new  Document()
 * val pid = id + "-" + paraCounter
 * System.err.println(s"pid: ${pid}")
 * System.err.println(s"text: ${p}")
 * titleAsDocument.add(new StringField("id", pid, Store.YES))
 * titleAsDocument.add(new TextField("text", title, Store.YES))
 * titleAsDocument.add(new IntField("isDoc", 0, Store.YES))
 * writer.addDocument(titleAsDocument)
 * } else {
 * val document = new Document()
 * val pid = id + "-" + paraCounter
 * System.err.println(s"pid: ${pid}")
 * System.err.println(s"text: ${p}")
 * document.add(new StringField("id", pid, Store.YES))
 * document.add(new IntField("isDoc", 0, Store.YES))
 * document.add(new TextField("text", p, Store.YES))
 * writer.addDocument(document)
 * }
 * }
 * }
 * }
 * }
 *
 * class PullAndAddPara (writer: IndexWriter) extends PullAndAdd(writer) {
 *
 * override def addXMLToDoc(writer: IndexWriter, buf: ArrayBuffer[String]): Unit ={
 * val x = XML.loadString(buf.mkString)
 * //    val x = buf.mkString
 * val id = (x \ "@id").text
 * //            System.err.println(s"id: ${id}")
 * val title = (x \ "@title").text
 * //    val firstpara = (x \ "@firstpara").text
 * val text = x.text
 * val paras = text.split("\n")
 * //    System.err.println(s"text: ${text}")
 * for ((p,paraCounter)<- paras.zipWithIndex) {
 * System.err.println(s"paracounter: ${paraCounter}")
 * if ((p == "") == false) {
 * if (paraCounter == 1) {
 * val wholeDocument = new Document()
 * System.err.println(s"pid: ${id}")
 * System.err.println(s"pid: ${text}")
 * wholeDocument.add(new StringField("id", id, Store.YES))
 * wholeDocument.add(new TextField("text", text, Store.YES))
 * wholeDocument.add(new IntField("isDoc", 1, Store.YES))
 * writer.addDocument(wholeDocument)
 * val titleAsDocument = new  Document()
 * val pid = id + "-" + paraCounter
 * System.err.println(s"pid: ${pid}")
 * System.err.println(s"text: ${p}")
 * titleAsDocument.add(new StringField("id", pid, Store.YES))
 * titleAsDocument.add(new TextField("text", title, Store.YES))
 * titleAsDocument.add(new IntField("isDoc", 0, Store.YES))
 * writer.addDocument(titleAsDocument)
 * } else {
 * val document = new Document()
 * val pid = id + "-" + paraCounter
 * System.err.println(s"pid: ${pid}")
 * System.err.println(s"text: ${p}")
 * document.add(new StringField("id", pid, Store.YES))
 * document.add(new IntField("isDoc", 0, Store.YES))
 * document.add(new TextField("text", p, Store.YES))
 * writer.addDocument(document)
 * }
 * }
 * }
 * }
 * }
 *
 */

/**
 * tf を常に1にするクラス
 * lucene で使われる
 */
class SimilarityWithConstantTF extends DefaultSimilarity {
  override def tf(freq: Float): Float = 1
  //override def lengthNorm(state: FieldInvertState): Float = 1
}

class SimilarityWithConstantIDF extends DefaultSimilarity {
  override def idf(freq: Long, numDocs: Long): Float = 1
  //override def lengthNorm(state: FieldInvertState): Float = 1
}

class SimilarityWithConstantNOM extends DefaultSimilarity {
  override def lengthNorm(state: FieldInvertState): Float = 1
  //override def lengthNorm(state: FieldInvertState): Float = 1
}

class PullFrTxtAndAdd(pageWriter: IndexWriter, sectWriter: IndexWriter, paraWriter: IndexWriter, sentWriter: IndexWriter) {
  val pageRe = """\[\[(.*?)\]\](?!\'\'\')""".r
  val sect1Re = """==([^=]*)==""".r
  val sect2Re = """===([^=]*)===""".r
  val sect3Re = """====([^=]*)====""".r
  val sect4Re = """=====([^=]*)=====""".r
  val sect5Re = """======([^=]*)======""".r
  val tableRe = """(?<=\{\{)[^\{\}]+(?=\}\})""".r
  val tableRemoveRe = """\{\{[^\{\}]*\}\}""".r
  //  val tableRe = """{{(.*)}}""".r

  def etcCollect(pageText: String): String =
    {
      //      println ("b4:"+pageText)
      val tableText = tableRe.findAllIn(pageText)
      if (tableText.hasDefiniteSize) "||" + pageText
      else {
        tableText.mkString("\n") + etcCollect(tableRemoveRe.replaceAllIn(pageText, "").mkString)
      }
    }

  def addParaNSentToDoc(paraWriter: IndexWriter, sentWriter: IndexWriter, textWoTable: String, id: String, file: BufferedWriter, ssw: BufferedWriter) = {
    val paraNumStream = Stream.iterate(1)(_ + 1).iterator
    file.write("<" + "text" + ">" + "\n")
    textWoTable.split("\n").map(p => {
      if ((p == "") == false) {
        val para = new Document()
        val paraID = id + "r" + paraNumStream.next().toString()
        addSentToDoc(sentWriter, p, paraID, ssw)
        //    print (pageText)
        //    System.err.println(s"pid: ${pageID}")
        //    System.err.println(s"ptext: ${pageText}")
        para.add(new StringField("id", paraID, Store.YES))
        para.add(new TextField("text", p, Store.YES))
        paraWriter.addDocument(para)
        file.write("<" + "para" + ">" + "\n")
        file.write("<" + "id" + ">" + "\n")
        file.write(paraID + "\n")
        file.write("</" + "id" + ">" + "\n")
        file.write("<" + "text" + ">" + "\n")
        file.write(StringEscapeUtils.escapeXml11(p) + "\n")
        file.write("</" + "text" + ">" + "\n")
        file.write("</" + "para" + ">" + "\n")
      }
    }
    )
    file.write("</" + "text" + ">" + "\n")

  }

  def addSentToDoc(sentWriter: IndexWriter, paraTextWoTable: String, id: String, ssw: BufferedWriter) = {
    val sentNumStream = Stream.iterate(1)(_ + 1).iterator
    paraTextWoTable.split("。").map(s => {
      if ((s == "") == false) {
        val sentence = new Document()
        val sentenceID = id + "t" + sentNumStream.next().toString()
        //    print (pageText)
        //    System.err.println(s"pid: ${pageID}")
        //    System.err.println(s"ptext: ${pageText}")
        sentence.add(new StringField("id", sentenceID, Store.YES))
        sentence.add(new TextField("text", s, Store.YES))
        System.err.println(s"s: [${s}]")
        ssw.write(s+"\n")
        sentWriter.addDocument(sentence)
      }
    }
    )
    ssw.close()
  }

  def addPageNSectToDoc(pageWriter: IndexWriter, sectWriter: IndexWriter, paraWriter: IndexWriter, buf: ArrayBuffer[String], pageID: String): Unit = {
    val file1 = new File("bugInIndexing1")
    val bw1 = new BufferedWriter(new FileWriter(file1))
    val xmlDir = new File("onePagePerFile")
    xmlDir.mkdir()
    val segmentedS = new File("/mnt/Works/wailoktam/segmentedS")
    val ssw = new BufferedWriter(new FileWriter(segmentedS))
    val file2 = new File("onePagePerFile/" + pageID + ".xml")
    val bw2 = new BufferedWriter(new FileWriter(file2))
    val file3 = new File("bugInIndexing3")
    val bw3 = new BufferedWriter(new FileWriter(file3))
    bw1.write(s"bufb4error: ${buf.mkString("\n")}")
    bw1.close()
    val x = XML.loadString(buf.mkString("\n"))

    val pageTitle = (x \\ "page" \ "@title").text
    //    val firstpara = (x \ "@firstpara").text

    val pageText = (x \\ "page")(0).child.collect { case Text(t) => t }.mkString("\n")
    val tableText = new StringBuilder
    tableText.append(etcCollect(pageText).split("\\|\\|")(0))
    val pageTextWoTable = etcCollect(pageText).split("\\|\\|")(1)
//    System.err.println(s"pageTextWoTable: ${pageTextWoTable}")
    val pageParaCount = pageTextWoTable.split("\n").filter { p => ((p == "") == false) }.length
    val fullText = new StringBuilder

    val mainPage = new Document()
//    System.err.println(s"id: ${pageID}")
//    System.err.println(s"pageParaCount: ${pageParaCount}")
    //    print (pageText)
    //    System.err.println(s"pid: ${pageID}")
    //    System.err.println(s"ptext: ${pageText}")

    mainPage.add(new StringField("id", pageID, Store.YES))
    mainPage.add(new TextField("title", pageTitle, Store.YES))
    mainPage.add(new TextField("text", pageTextWoTable, Store.YES))
    mainPage.add(new IntField("paras", pageParaCount, Store.YES))
    sectWriter.addDocument(mainPage)
    fullText.append(pageTextWoTable)
    bw2.write("<" + "page" + ">" + "\n")
    bw2.write("<" + "id" + ">" + "\n")
    bw2.write(pageID + "\n")
    bw2.write("</" + "id" + ">" + "\n")
    bw2.write("<" + "title" + ">" + "\n")
    bw2.write(StringEscapeUtils.escapeXml11(pageTitle) + "\n")
    bw2.write("</" + "title" + ">" + "\n")

    addParaNSentToDoc(paraWriter, sentWriter, pageTextWoTable, pageID, bw2, ssw)

    if ((x \\ "section1") != "") {
      val sect1NumStream = Stream.iterate(1)(_ + 1).iterator
      for (section1 <- (x \\ "section1")) {
        val sect1ID = pageID + "s" + sect1NumStream.next().toString()

        val sect1 = new Document()
        //space killed?
        val sect1Title = (section1 \ "@title").text
        val sect1Text = section1.child.collect { case Text(t) => t }.mkString("\n")
        val sect1TextWoTable = etcCollect(sect1Text).split("\\|\\|")(1)
        tableText.append(etcCollect(sect1Text).split("\\|\\|")(0))
        val sect1ParaCount = sect1TextWoTable.split("\n").filter { p => ((p == "") == false) }.length
//        System.err.println(s"id: ${sect1ID}")
//        System.err.println(s"sect1ParaCount: ${sect1ParaCount}")
        sect1.add(new StringField("id", sect1ID, Store.YES))
        sect1.add(new TextField("title", sect1Title, Store.YES))
        sect1.add(new TextField("text", sect1TextWoTable, Store.YES))
        sect1.add(new IntField("paras", sect1ParaCount, Store.YES))
        sectWriter.addDocument(sect1)
        fullText.append(sect1TextWoTable)
        bw2.write("<" + "sect1" + ">" + "\n")
        bw2.write("<" + "id" + ">" + "\n")
        bw2.write(sect1ID + "\n")
        bw2.write("</" + "id" + ">" + "\n")
        bw2.write("<" + "title" + ">" + "\n")
        bw2.write(StringEscapeUtils.escapeXml11(sect1Title) + "\n")
        bw2.write("</" + "title" + ">" + "\n")
        addParaNSentToDoc(paraWriter, sentWriter, sect1TextWoTable, sect1ID, bw2, ssw)
        if ((section1 \ "section2") != "") {
          val sect2NumStream = Stream.iterate(1)(_ + 1).iterator
          for (section2 <- (section1 \ "section2")) {
            val sect2ID = sect1ID + "s" + sect2NumStream.next().toString()
            val sect2 = new Document()
            val sect2Title = (section2 \ "@title").text
            val sect2Text = section2.child.collect { case Text(t) => t }.mkString("\n")
            val sect2TextWoTable = etcCollect(sect2Text).split("\\|\\|")(1)
            tableText.append(etcCollect(sect2Text).split("\\|\\|")(0))
            val sect2ParaCount = sect2TextWoTable.split("\n").filter { p => ((p == "") == false) }.length
//            System.err.println(s"id: ${sect2ID}")
//            System.err.println(s"sect2ParaCount: ${sect2ParaCount}")
            sect2.add(new StringField("id", sect2ID, Store.YES))
            sect2.add(new TextField("title", sect2Title, Store.YES))
            sect2.add(new TextField("text", sect2TextWoTable, Store.YES))
            sect2.add(new IntField("paras", sect2ParaCount, Store.YES))
            sectWriter.addDocument(sect2)
            fullText.append(sect2TextWoTable)
            bw2.write("<" + "sect2" + ">" + "\n")
            bw2.write("<" + "id" + ">" + "\n")
            bw2.write(sect2ID + "\n")
            bw2.write("</" + "id" + ">" + "\n")
            bw2.write("<" + "title" + ">" + "\n")
            bw2.write(StringEscapeUtils.escapeXml11(sect2Title) + "\n")
            bw2.write("</" + "title" + ">" + "\n")
            addParaNSentToDoc(paraWriter, sentWriter, sect2TextWoTable, sect2ID, bw2, ssw)
            //            print("sect2"+sect2ID+"\n")
            //            print("sect2"+sect2Text+"\n")
            if ((section2 \ "section3") != "") {
              val sect3NumStream = Stream.iterate(1)(_ + 1).iterator
              for (section3 <- (section2 \ "section3")) {
                val sect3ID = sect2ID + "s" + sect3NumStream.next().toString()
                val sect3 = new Document()
                val sect3Title = (section3 \ "@title").text
                val sect3Text = section3.child.collect { case Text(t) => t }.mkString("\n")
                val sect3TextWoTable = etcCollect(sect3Text).split("\\|\\|")(1)
                tableText.append(etcCollect(sect3Text).split("\\|\\|")(0))
                val sect3ParaCount = sect3TextWoTable.split("\n").filter { p => ((p == "") == false) }.length
//                System.err.println(s"id: ${sect3ID}")
//                System.err.println(s"sect3ParaCount: ${sect3ParaCount}")
                sect3.add(new StringField("id", sect3ID, Store.YES))
                sect3.add(new TextField("title", sect3Title, Store.YES))
                sect3.add(new TextField("text", sect3TextWoTable, Store.YES))
                sect3.add(new IntField("paras", sect3ParaCount, Store.YES))
                sectWriter.addDocument(sect3)
                fullText.append(sect3TextWoTable)
                bw2.write("<" + "sect3" + ">" + "\n")
                bw2.write("<" + "id" + ">" + "\n")
                bw2.write(sect3ID + "\n")
                bw2.write("</" + "id" + ">" + "\n")
                bw2.write("<" + "title" + ">" + "\n")
                bw2.write(StringEscapeUtils.escapeXml11(sect3Title) + "\n")
                bw2.write("</" + "title" + ">" + "\n")
                addParaNSentToDoc(paraWriter, sentWriter, sect3TextWoTable, sect3ID, bw2, ssw)
                if ((section3 \ "section4") != "") {
                  val sect4NumStream = Stream.iterate(1)(_ + 1).iterator
                  for (section4 <- (section3 \ "section4")) {
                    val sect4ID = sect3ID + "s" + sect4NumStream.next().toString()
                    val sect4 = new Document()
                    val sect4Title = (section4 \ "@title").text
                    val sect4Text = section4.child.collect { case Text(t) => t }.mkString("\n")
                    val sect4TextWoTable = etcCollect(sect4Text).split("\\|\\|")(1)
                    tableText.append(etcCollect(sect4Text).split("\\|\\|")(0))
                    val sect4ParaCount = sect4TextWoTable.split("\n").filter { p => ((p == "") == false) }.length
                    //                    print(sect4Text)
//                    System.err.println(s"id: ${sect4ID}")
//                    System.err.println(s"sect4ParaCount: ${sect4ParaCount}")
                    sect4.add(new StringField("id", sect4ID, Store.YES))
                    sect4.add(new TextField("title", sect4Title, Store.YES))
                    sect4.add(new TextField("text", sect4TextWoTable, Store.YES))
                    sect4.add(new IntField("paras", sect4ParaCount, Store.YES))
                    sectWriter.addDocument(sect4)
                    fullText.append(sect4TextWoTable)
                    bw2.write("<" + "sect4" + ">" + "\n")
                    bw2.write("<" + "id" + ">" + "\n")
                    bw2.write(sect4ID + "\n")
                    bw2.write("</" + "id" + ">" + "\n")
                    bw2.write("<" + "title" + ">" + "\n")
                    bw2.write(StringEscapeUtils.escapeXml11(sect4Title) + "\n")
                    bw2.write("</" + "title" + ">" + "\n")
                    addParaNSentToDoc(paraWriter, sentWriter, sect4TextWoTable, sect4ID, bw2,ssw)
                    if ((section4 \ "section5") != "") {
                      val sect5NumStream = Stream.iterate(1)(_ + 1).iterator
                      for (section5 <- (section4 \ "section5")) {
                        val sect5ID = sect4ID + "s" + sect5NumStream.next().toString()
                        val sect5 = new Document()
                        val sect5Title = (section5 \ "@title").text
                        val sect5Text = section5.child.collect { case Text(t) => t }.mkString("\n")
                        val sect5TextWoTable = etcCollect(sect5Text).split("\\|\\|")(1)
                        tableText.append(etcCollect(sect5Text).split("\\|\\|")(0))
                        val sect5ParaCount = sect5TextWoTable.split("\n").filter { p => ((p == "") == false) }.length
//                        System.err.println(s"id: ${sect5ID}")
//                        System.err.println(s"sect5ParaCount: ${sect5ParaCount}")
                        sect5.add(new StringField("id", sect5ID, Store.YES))
                        sect5.add(new TextField("title", sect5Title, Store.YES))
                        sect5.add(new TextField("text", sect5TextWoTable, Store.YES))
                        sect5.add(new IntField("paras", sect5ParaCount, Store.YES))
                        sectWriter.addDocument(sect5)
                        fullText.append(sect5TextWoTable)
                        bw2.write("<" + "sect5" + ">" + "\n")
                        bw2.write("<" + "id" + ">" + "\n")
                        bw2.write(sect5ID + "\n")
                        bw2.write("</" + "id" + ">" + "\n")
                        bw2.write("<" + "title" + ">" + "\n")
                        bw2.write(StringEscapeUtils.escapeXml11(sect5Title) + "\n")
                        bw2.write("</" + "title" + ">" + "\n")
                        addParaNSentToDoc(paraWriter, sentWriter, sect5TextWoTable, sect5ID, bw2,ssw)
                        bw2.write("</" + "sect5" + ">" + "\n")
                      }
                    }
                    bw2.write("</" + "sect4" + ">" + "\n")
                  }
                }
                bw2.write("</" + "sect3" + ">" + "\n")
              }
            }
            bw2.write("</" + "sect2" + ">" + "\n")
          }
        }
        bw2.write("</" + "sect1" + ">" + "\n")
      }
    }

    val tableEtc = new Document()
    //    print (pageText)
    //    System.err.println(s"pid: ${pageID}")
    //    System.err.println(s"ptext: ${pageText}")
    tableEtc.add(new StringField("id", pageID.toString() + "-0", Store.YES))
    tableEtc.add(new IntField("paras", 0, Store.YES))
    tableEtc.add(new TextField("text", tableText.result, Store.YES))
    sectWriter.addDocument(tableEtc)
    fullText.append(tableText.result)
    val fullPage = new Document()
    //    System.err.println(s"id: ${pageID}")
    //    System.err.println(s"pageParaCount: ${pageParaCount}")
    //    print (pageText)
    //    System.err.println(s"pid: ${pageID}")
    //    System.err.println(s"ptext: ${pageText}")

    fullPage.add(new StringField("id", pageID, Store.YES))
    fullPage.add(new TextField("title", pageTitle, Store.YES))
    fullPage.add(new TextField("text", fullText.result(), Store.YES))
    pageWriter.addDocument(fullPage)
    bw2.write("<" + "table" + ">" + "\n")
    bw2.write(StringEscapeUtils.escapeXml11(tableText.result) + "\n")
    bw2.write("</" + "table" + ">" + "\n")
    bw2.write("</" + "page" + ">" + "\n")
    //    print("page"+pageID+"\n")
    //    print ("pageText"+pageText+"\n")
    //    print("page"+pageTextWoTable+"\n")
    //    print("table"+tableText+"\n")
    bw2.close()
    ssw.close()
    XML.loadFile(file2)
  }
  /**
   * val paras = text.split("\n")
   * //    System.err.println(s"text: ${text}")
   * for ((p,paraCounter)<- paras.zipWithIndex) {
   * System.err.println(s"paracounter: ${paraCounter}")
   * if ((p == "") == false) {
   * if (paraCounter == 1) {
   *
   * val titleAsDocument = new  Document()
   * val pid = id + "-" + paraCounter
   * System.err.println(s"pid: ${pid}")
   * System.err.println(s"text: ${p}")
   * titleAsDocument.add(new StringField("id", pid, Store.YES))
   * titleAsDocument.add(new TextField("text", title, Store.YES))
   * titleAsDocument.add(new IntField("isDoc", 0, Store.YES))
   * writer.addDocument(titleAsDocument)
   * } else {
   * val document = new Document()
   * val pid = id + "-" + paraCounter
   * System.err.println(s"pid: ${pid}")
   * System.err.println(s"text: ${p}")
   * document.add(new StringField("id", pid, Store.YES))
   * document.add(new IntField("isDoc", 0, Store.YES))
   * document.add(new TextField("text", p, Store.YES))
   * writer.addDocument(document)
   */

  def apply(fileName: String, fileID: String) = {

    val bufferedSource = Source.fromFile(fileName)
    val lines = bufferedSource.getLines
    var pageFound = 0
    var sect1Found = 0
    var sect2Found = 0
    var sect3Found = 0
    var sect4Found = 0
    var sect5Found = 0
    var firstPage = 1
    var buf = ArrayBuffer[String]()
    val pageNumStream = Stream.iterate(1)(_ + 1).iterator
    //    val pageID = pageNumStream.next
    //    val file4 = new File("bugInIndexing4")
    //    val bw4 = new BufferedWriter(new FileWriter(file4))
    for (line <- lines) {
      line match {
        case pageRe(pageTitle) => {
          if (sect5Found == 1) {
            buf += "</" + "section5" + ">"
            sect5Found = 0
          }
          if (sect4Found == 1) {
            buf += "</" + "section4" + ">"
            sect4Found = 0
          }
          if (sect3Found == 1) {
            buf += "</" + "section3" + ">"
            sect3Found = 0
          }
          if (sect2Found == 1) {
            buf += "</" + "section2" + ">"
            sect2Found = 0
          }
          if (sect1Found == 1) {
            buf += "</" + "section1" + ">"
            sect1Found = 0
          }
          if (pageFound == 1) {
            val pageID = pageNumStream.next
            buf += "</" + "page" + ">"
//            System.err.println(s"fileID and pageID first: ${fileID + "f" + pageID.toString()}")
            addPageNSectToDoc(pageWriter, sectWriter, paraWriter, buf, fileID + "f" + pageID.toString())
            buf = ArrayBuffer[String]()
          }
          buf += "<" + "page" + " title=" + "\"" + StringEscapeUtils.escapeXml11(pageTitle) + "\"" + ">"
          pageFound = 1
        }
        case sect1Re(sect1Title) => {
          if (sect5Found == 1) {
            buf += "</" + "section5" + ">"
            sect5Found = 0
          }
          if (sect4Found == 1) {
            buf += "</" + "section4" + ">"
            sect4Found = 0
          }
          if (sect3Found == 1) {
            buf += "</" + "section3" + ">"
            sect3Found = 0
          }
          if (sect2Found == 1) {
            buf += "</" + "section2" + ">"
            sect2Found = 0
          }
          if (sect1Found == 1) {
            buf += "</" + "section1" + ">"
          }
          buf += "<" + "section1" + " title=" + "\"" + StringEscapeUtils.escapeXml11(sect1Title) + "\"" + ">"
          sect1Found = 1
        }
        case sect2Re(sect2Title) => {
          if (sect5Found == 1) {
            buf += "</" + "section5" + ">"
            sect5Found = 0
          }
          if (sect4Found == 1) {
            buf += "</" + "section4" + ">"
            sect4Found = 0
          }
          if (sect3Found == 1) {
            buf += "</" + "section3" + ">"
            sect3Found = 0
          }
          if (sect2Found == 1) {
            buf += "</" + "section2" + ">"
            sect2Found = 0
          }
          buf += "<" + "section2" + " title=" + "\"" + StringEscapeUtils.escapeXml11(sect2Title) + "\"" + ">"
          sect2Found = 1
        }
        case sect3Re(sect3Title) => {
          if (sect5Found == 1) {
            buf += "</" + "section5" + ">"
            sect5Found = 0
          }
          if (sect4Found == 1) {
            buf += "</" + "section4" + ">"
            sect4Found = 0
          }
          if (sect3Found == 1) {
            buf += "</" + "section3" + ">"
            sect3Found = 0
          }
          buf += "<" + "section3" + " title=" + "\"" + StringEscapeUtils.escapeXml11(sect3Title) + "\"" + ">"
          sect3Found = 1
        }
        case sect4Re(sect4Title) => {
          if (sect5Found == 1) {
            buf += "</" + "section5" + ">"
            sect5Found = 0
          }
          if (sect4Found == 1) {
            buf += "</" + "section4" + ">"
            sect4Found = 0
          }
          buf += "<" + "section4" + " title=" + "\"" + StringEscapeUtils.escapeXml11(sect4Title) + "\"" + ">"
          sect4Found = 1
        }
        case sect5Re(sect5Title) => {
          if (sect5Found == 1) {
            buf += "</" + "section5" + ">"
            sect5Found = 0
          }
          buf += "<" + "section5" + " title=" + "\"" + StringEscapeUtils.escapeXml11(sect5Title) + "\"" + ">"
          sect5Found = 1
        }

        case _ =>
          {
            buf += StringEscapeUtils.escapeXml11(line)
            /**
             * if (i == (lines.size-1)) {
             * if (sect5Found == 1 ) {
             * buf += "</" + "section5"+  ">"
             * sect5Found = 0
             * }
             * if (sect4Found == 1 ) {
             * buf += "</" + "section4"+  ">"
             * sect4Found = 0
             * }
             * if (sect3Found == 1 ) {
             * buf += "</" + "section3"+  ">"
             * sect3Found = 0
             * }
             * if (sect2Found == 1 ) {
             * buf += "</" + "section2"+  ">"
             * sect2Found = 0
             * }
             * if (sect1Found == 1 ) {
             * buf += "</" + "section1"+  ">"
             * sect1Found = 0
             * }
             * if (pageFound == 1) {
             * val pageID = pageNumStream.next
             * buf += "</" + "page"+">"
             * addPageToDoc(pageWriter, paraWriter, buf, pageID)
             * buf = ArrayBuffer[String]()
             * }
             * }
             */
          }
      }
    }
    if (sect5Found == 1) {
      buf += "</" + "section5" + ">"
      sect5Found = 0
    }
    if (sect4Found == 1) {
      buf += "</" + "section4" + ">"
      sect4Found = 0
    }
    if (sect3Found == 1) {
      buf += "</" + "section3" + ">"
      sect3Found = 0
    }
    if (sect2Found == 1) {
      buf += "</" + "section2" + ">"
      sect2Found = 0
    }
    if (sect1Found == 1) {
      buf += "</" + "section1" + ">"
      sect1Found = 0
    }
    if (pageFound == 1) {
      val pageID = pageNumStream.next
      buf += "</" + "page" + ">"
      //      bw4.write(pageID+"\n")
//      System.err.println(s"fileID And pageID final: ${fileID + "f" + pageID.toString()}")
      addPageNSectToDoc(pageWriter, sectWriter, paraWriter, buf, fileID + "f" + pageID.toString)
      buf = ArrayBuffer[String]()
    }
    //    val titleIdxs = (0 until wholeFile.size).filter {
    //      i =>
    //      wholeFile(i) match titleRe && (i > 0 && wholeFile(i-1)=="" )
    //    }

    //    val wholeFile = bufferedSource.getLines.mkString("\n")
    //    val pages = wholeFile.split("\n\\[\\[.*\\]\\]\n")
    //    for (page <- titles) bw.write("page start\n" + page+"page end\n")
    //    for (page <- pages) bw.write("page start\n" + page+"page end\n")
    //    bw.close()
    bufferedSource.close

  }

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

  /**
   * for output of wikipediaExtractor
   * @param fileName
   * @return
   */

  def cleanFile(fileName: String): Array[String] = {
    val docTagRe = """<\/?doc.*?>""".r
    val openDocTagRe = """<doc.*?>""".r
    val captureTitleRe = """<doc[^>]*title\s*=\s*"([^"]*)">""".r
    var capturedTitle = ""
    val captureOpenDocTagContenteRe = """(<doc[^>]*)>""".r
    var openTagLineNo = 0
    var titleFound = 0
    var firstParaFound = 0
    var firstPara = ""
    val fileTagRe = """<\/?file.*?>""".r
    val nonDocTagRe = """<(?!\/?doc(?=>|\s.*>))\/?.*?>""".r
    val bufferedSource = Source.fromFile(fileName)
    val lines = bufferedSource.getLines.toArray
    //System.err.println(s"lines: ${lines.mkString("\n")}")
    //    System.err.println(s"linesb4: ${lines.mkString}")
    //    System.err.println(s"linesLength: ${Source.fromFile(fileName).getLines.length}")
    //    System.err.println(s"linesb4: ${lines}")
    val outLines = new Array[String](lines.length)
    //    for (line <- lines) yield {
    for ((line, lineNo) <- lines.zipWithIndex) yield {
      //      System.err.println(s"lineb4: ${line}")
      if (fileTagRe.findFirstIn(line) != None) {
        outLines(lineNo) = nonDocTagRe.replaceAllIn(line, "")
      } else {
        if (docTagRe.findFirstIn(line) != None) {
          //          System.err.println(s"hit line: ${line}")
          if (openDocTagRe.findFirstIn(line) != None) {
            val captureTitleRe(title) = line
            capturedTitle = title
            openTagLineNo = lineNo
          } else {
            titleFound = 0
            firstParaFound = 0
          }
          outLines(lineNo) = line

        } else {
          val titleInDoc = capturedTitle
          val firstParaRe = """\S+[^\n]+""".r
          //          System.err.println(s"titleInDoc: ${capturedTitle.mkString}")
          //          System.err.println(s"line: ${line.mkString}")
          //          System.err.println(s"titleFound: ${titleFound}")
          if ((line contains titleInDoc) && (titleFound == 0)) {
            //            System.err.println(s"titleline: ${line.mkString}")
            titleFound = 1
          } else {
            line match {
              case firstParaRe() if (titleFound == 1) && (firstParaFound == 0) => {
                val captureOpenDocTagContenteRe(content) = outLines(openTagLineNo)
                //                System.err.println(s"content: ${content.mkString}")
                outLines(openTagLineNo) = content + " firstpara=\"" + StringEscapeUtils.escapeXml11(line) + "\">"
                //                System.err.println(s"rewritten: ${outLines(openTagLineNo).mkString}")
                firstParaFound = 1
              }
              case _ =>
            }
          }
          //        if (nonDocTagRe.findFirstIn(line) != None) {
          //          System.err.println(s"hit: ${line}")

          //        }
          //          outLines(lineNo) = line
          outLines(lineNo) = StringEscapeUtils.escapeXml11(line)
        }
      }
    }
    bufferedSource.close
    //    System.err.println(s"outlines: ${outLines.mkString("\n")}")
    outLines
  }

  //  private def makeIndexMain(knowledgeFiles: Array[String], indexDir: Directory): Array[(String, Elem)] = {
  def makeIndexMain(knowledgeFiles: Array[String], indexDirName: String): Unit = {
    val pageIndexDir = FSDirectory.open(new File(indexDirName + "/page"))
    val sectIndexDir = FSDirectory.open(new File(indexDirName + "/sect"))
    val paraIndexDir = FSDirectory.open(new File(indexDirName + "/para"))
    val sentIndexDir = FSDirectory.open(new File(indexDirName + "/sent"))
    //    val debugList = List("/home/wailoktam/qa/input/knowledge/rite2-ja-textbook.xml","/home/wailoktam/qa/input/knowledge/riteval-ja-textbook2.xml", "/home/wailoktam/qa/input/knowledge/wiki_00")
    val analyzer = new JapaneseAnalyzer()
    val config1 = new IndexWriterConfig(Version.LUCENE_4_10_0, analyzer)
    config1.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    config1.setSimilarity(new SimilarityWithConstantTF)
    val config2 = new IndexWriterConfig(Version.LUCENE_4_10_0, analyzer)
    config2.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    config2.setSimilarity(new SimilarityWithConstantTF)
    val config3 = new IndexWriterConfig(Version.LUCENE_4_10_0, analyzer)
    config3.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    config3.setSimilarity(new SimilarityWithConstantTF)
    val config4 = new IndexWriterConfig(Version.LUCENE_4_10_0, analyzer)
    config4.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    config4.setSimilarity(new SimilarityWithConstantTF)
    val pageWriter = new IndexWriter(pageIndexDir, config1) // overwrite existing index
    val sectWriter = new IndexWriter(sectIndexDir, config2) // overwrite existing index
    val paraWriter = new IndexWriter(paraIndexDir, config3) // overwrite existing index
    val sentWriter = new IndexWriter(sentIndexDir, config4)
    val pullAndAddInstance = new PullFrTxtAndAdd(pageWriter, sectWriter, paraWriter, sentWriter)
    val fileNumStream = Stream.iterate(1)(_ + 1).iterator
    //    var id = 0
    /**
     * for (knowledgeFile <- knowledgeFiles) yield {
     * System.err.println(s"preprocessing file: ${knowledgeFile}")
     * val contentToWrite = "<file>\n" + cleanFile(knowledgeFile).mkString("\n") + "\n</file>"
     * val bw = new java.io.BufferedWriter(new java.io.FileWriter(new File(knowledgeFile)))
     * bw.write(contentToWrite)
     * bw.close()
     * }
     */
    for (knowledgeFile <- knowledgeFiles) yield {
      val fileID = fileNumStream.next
      System.err.println(s"fileID: ${fileID}")
      pullAndAddInstance(knowledgeFile, fileID.toString())
    }
    //    pullAndAddPageInstance = new pullAndAddPage(sectWriter)
    //    pullAndAddParaInstance = new pullAndAddPara(parawriter)
    //    knowledgeFiles.map(pullAndAddPageInstance(_))
    //    knowledgeFiles.map(pullAndAddParaInstance(_))
    //  val docXmlPairs = knowledgeFiles.map(pullAndAdd(writer,_)).flatten.toArray
    pageWriter.close
    sectWriter.close
    paraWriter.close
    sentWriter.close
    //    docXmlPairs

  }


  /**
   * def makeIndexOnFile(target_file_names: Array[String], index_dir_name: String, document_cdb_name: String): Directory = {
   * val index_dir = FSDirectory.open(new File(index_dir_name))
   * val documents = makeIndexMain(target_file_names, index_dir)
   * val cdb_make = new CDBMapMake()
   * cdb_make.open(document_cdb_name)
   * documents foreach { d: (String, Elem) => cdb_make.add(d._1, d._2.toString) }
   * cdb_make.close()
   * index_dir
   * }
   */

  def apply(knowDirName: String, indexDirName: String): Unit = {

    makeIndexMain(SharedFunctions.makeFileList(knowDirName), indexDirName)
    //    (indexDir, documents.toMap)
  }
}

object Indexing {

  //  def main(args: Array[String]): Unit = {
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
    createIndex(args(0), args(1))
    println("done")
    // try search

  }
}


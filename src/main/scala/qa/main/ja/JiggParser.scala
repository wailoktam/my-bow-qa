package qa.main.ja

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import com.ibm.icu.text.Transliterator
import scala.util.matching.Regex
import sys.process._
import java.io.File
import scala.xml.factory.XMLLoader
import scala.xml._
import scala.io.Source

object XMLLoaderIgnoringDTD extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.setValidating(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}

object QuestionTypeQ1000 extends Enumeration {
  val how_many, // choose a true statement
  when, // choose a false statement
  where, // choose a combination of true/false
  what, who, what_kind_of, other // other (not answered by this system)
  = Value
}



case class Question(id: String, // 回答欄ID
                    questionType: QuestionTypeQ1000.Value, // 問題文のタイプ
                    parses: Array[Elem],
//                    arraryOfClues: Array[Clue],
                    questionText: NodeSeq,
//                    answerNumber: Int,
                    meta: NodeSeq,
                    answers: NodeSeq
                     )

object ExtractQuestionsQ1000 {
  /**
   * using metadata for determining question type
   */
  def determineQuestionTypeQ1000(questionXML: Node, parserPath: String) = {
    val whereRe = """(どこ)""".r
    val whenRe = """(いつ)""".r
    val howManyRe = """(いくつ|いくら|どれぐらい)""".r
    val whatKindOfRe = """(どんな)""".r
    val isRe = """(は)""".r
    val whatRe = """(なに+)""".r
    val whatNotUnitRe = """(なに\+ひらがな)""".r
    val howManyUnitRe = """(なに\+漢字)""".r
    val whichYearRe = """(何年)""".r
    val howManyPplRe = """(何人)""".r
    val whoRe = """(誰)""".r
    val dimensionRe = """(数|体積|長さ|温度|頻度)""".r
    val timeExprRe = """(日付表現)""".r
    (questionXML \\ "B1").text match {
      case whereRe(questionWord) => QuestionTypeQ1000.where
      case howManyRe(questionWord) => QuestionTypeQ1000.how_many
      case whenRe(questionWord) => QuestionTypeQ1000.when
      case whatKindOfRe(questionWord) => QuestionTypeQ1000.what_kind_of
      case isRe(questionWord) => QuestionTypeQ1000.what
      case whatRe(questionWord) => QuestionTypeQ1000.what
      case whatNotUnitRe(questionWord) => QuestionTypeQ1000.what
      case howManyUnitRe(questionWord) =>
        if (dimensionRe.findFirstMatchIn((questionXML \\ "D3").text) != None)
          QuestionTypeQ1000.how_many
        else
          QuestionTypeQ1000.what
      case whichYearRe(questionWord) =>
        if (timeExprRe.findFirstMatchIn((questionXML \\ "D3").text) != None)
          QuestionTypeQ1000.when
        else
          QuestionTypeQ1000.how_many
      case howManyPplRe(questionWord) => QuestionTypeQ1000.how_many
      case whoRe(questionWord) => QuestionTypeQ1000.who
      case _ => QuestionTypeQ1000.other
    }
  }



  def countAnswerNoQ1000(questionXML: Node): Int = {
    var increment = 0
    for (answer <- (questionXML \\ "answer")) yield {
      increment += 1
    }
    increment
  }

  def parseQuestion(questionXML: Node,parserPath: String):Array[Elem]={
    val jigg = new JiggParser(parserPath)
    val parses = jigg.parse((questionXML \\ "text").text)
    parses
  }

  def makeQuestionQ1000(questionXML: Node, parserPath: String) = {
    val parses = parseQuestion(questionXML, parserPath)
    Question((questionXML \ "@id").text,
      determineQuestionTypeQ1000(questionXML, parserPath),
      parses,
//      extractCluesQ1000(questionXML,parses),
      questionXML \\ "text",
      //      countAnswerNoQ1000(questionXML),
      questionXML \\ "meta",
      questionXML \\ "answers")
  }

  def safeMod5(stringIn: String): Boolean = {
    var boolOut = true
    val idRe = """LC-ECQA2002-(\d+)-01""".r

    stringIn match {
      case idRe(id) =>
        System.err.println(s"mod5: ${id.toInt % 5}, ${id.toInt}")
        if (id.toInt % 5 == 0)
          boolOut = false
    }
    boolOut
  }

  def formatInXML(question: Question):Elem ={
    question match {
      case Question(id,questionType,parses,questionText,meta,answers) =>
        <question id={id}>
          {questionText}
          {meta}
          {answers}
          <annotations>
          <annotation type="juman/knp" annotator="JiggParser">
          <questionType>{questionType}</questionType>
          <parses>{for (parse <- parses) yield <parse>
            {parse}
            </parse>}
          </parses>
          </annotation>
          </annotations>

        </question>
    }
  }

  def apply(inputXML: Node, parserPath: String): Array[Elem] = {
    val totalQuestions = (inputXML \\ "question").filter(e => (safeMod5((e \ "@id").text) == true)).toArray
    System.err.println(s"Total questions: ${totalQuestions.length}")
    totalQuestions map (q => makeQuestionQ1000(q, parserPath)) map (formatInXML)
  }
}

// Interface to run Jigg
class JiggParser(val parserPath: String) {


  def normalize(text: String): String = {
    val chars =
      text.replaceAll("""[\t 　]+""", "") map { // スペースは削除
        // 半角を全角に変換
        c =>
          if ((('A' to 'Z') contains c) ||
            (('a' to 'z') contains c) ||
            (('1' to '9') contains c) ||
            ("""0!"#$%&'()=~|-^¥\@{}[]+*;:<>?,./""" contains c)) {
            (c + 'Ａ' - 'A').asInstanceOf[Char]
          } else c
      } map {
        _ match {
          case '／' => '・' // ／ を ・ に変換（その後、・は coordination として扱われる）
          case c   => c
        }
      }
    val normalized_text = new String(chars.toArray)
    normalized_text
  }






  def runJigg(parserPath: String, inputStringParam: String):Elem  = {
    val inputString = "echo " + inputStringParam
    val jiggCommand = "java -cp "+ parserPath + " jigg.pipeline.Pipeline -annotators ssplit,juman,knp"
    System.err.println(s"jiggComman: ${inputString}|${jiggCommand}")
    val jiggOutput = XMLLoaderIgnoringDTD.loadString((inputString #| jiggCommand).!!)
    jiggOutput
  }



  def parse(text: String): Array[Elem] = {
    // sentence spliting
    // currently, newline and "。" is regarded as sentence boundaries
    val sentences: Array[String] = normalize(text).split("""\n+|。\n*""").map(_ + "。")
    val jigg_outputs = sentences map {k=>runJigg(parserPath,k)}
    jigg_outputs
  }
}

object JiggParser {
  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      System.err.println("Usage: scala qa.main.ja.JiggParser JIGG_PATH INPUT_XML OUTPUT_XML")
      System.exit(1)
    }
    val elems = ExtractQuestionsQ1000(XMLLoaderIgnoringDTD.loadFile(args(1)), args(0))
    XML.save(args(2), <questions>
      {for (elem <- elems) yield {
        elem
      }}
      </questions>, "UTF-8")
  }
}


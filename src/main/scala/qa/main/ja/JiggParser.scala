// Wrapper for Juman/KNP
// KNP.parse parses a text into KNP parse trees （KNP解析木)

package qa.main.ja

//import java.io.{ OutputStreamWriter, BufferedWriter, InputStreamReader, BufferedReader }

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
    //f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
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

case class Clue(headPred: String,
                deps: List[String],
                parse: String)

case class Question(id: String, // 回答欄ID
                    questionType: QuestionTypeQ1000.Value, // 問題文のタイプ
                    parses: Array[Elem],
                    arraryOfClues: Array[Clue],
                    questionText: NodeSeq,
//                    answerNumber: Int,
                    meta: NodeSeq,
                    answers: NodeSeq)

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

  def predIsHead(parseXML:Node, headId: String):Boolean = {
    if ((parseXML \\ "@predicate").text == headId) true
    else false
  }

  def argIsHead(parseXML:Node, altHeadIds: Seq[String]):Boolean ={
    val altHeadIdsRe = altHeadIds.mkString("|").r
    (parseXML \\ "@argument").text match  {
      case altHeadRe => true
      case _ => false
    }
  }

  def findIdOfGivenHead(node: Node, givenHead:String):Boolean = {
    val lexemeExtractorRe = """.*?<正規化代表表記:(.+?)/.+>.*""".r
    System.err.println(s"givenHead: ${givenHead}")
    (node \\ "@features").text match {
      case lexemeExtractorRe(lexeme) if (lexeme == givenHead) => true
      case _ => false
    }
  }

  def predOrArgIsHead(parseXML:Node, headId: String, altHeadIds: Seq[String]): Boolean = {
    if ((predIsHead(parseXML, headId) == true) || (argIsHead(parseXML, altHeadIds)==true))
      true
    else false
  }

  def findLexemebyId(id:String, parseXML:Node):String = {
    val lexemeExtractorRe = """.*?<正規化代表表記:(.+?)/.+>.*""".r
    ((parseXML \\ "basicPhrase").find(n=> (n\"@id").text == id).getOrElse(<dummy></dummy>)\\"@features").text match {
      case lexemeExtractorRe(lexeme) => {
        System.err.println(s"headPredlexeme: ${lexeme}")
        lexeme
      }
      case _ => {
        System.err.println(s"failedHeadPredLexeme: ${parseXML}")
        System.err.println(s"failedHeadPredLexeme: ${(parseXML \\ "basicPhrase").find(n=> (n\"@id").text == id)}")
        ""
      }
    }
  }



  def findLexemebyAltId(altId:String, parseXML:Node):String ={
    val lexemes = (parseXML \\ "coreference")filter{n=>(n\\"@id").text==altId}map(n=> (n\\"@basicPhrases").text )map(s=>findLexemebyId(s,parseXML))
    if (lexemes.length == 1) lexemes.head
    else ""
  }

  def makeClue(semRels:Seq[Node], parseXML:Node): Array[Clue] ={
    var clueMap = collection.mutable.Map[String, List[String]]()
    for (semRel <- semRels) yield {
      if (clueMap.contains((semRel \\ "@predicate" ).text) == false){
        clueMap((semRel\\"@predicate").text) = List ((semRel\\"@argument").text)
      }
      else clueMap((semRel\\"@predicate").text) = ((semRel\\"@argument").text)::(clueMap((semRel \\  "@predicate").text))
    }
    clueMap.map(t=>Clue(findLexemebyId(t._1,parseXML),t._2.map(s=>findLexemebyAltId(s,parseXML)),(parseXML\\"sentence").text)).toArray
  }




  /**
   * using meta data to determine clues
   */



  def extractCluesQ1000(questionXML: Node, parses: Array[Elem]): Array[Clue] = {
    val clues = for (parse <- parses) yield {
    val headId =  (((parse \\ "basicPhrase").find(n=>findIdOfGivenHead(n,(questionXML \\ "B7").text))).getOrElse(<dummy></dummy>)\\"@id").text
    val altHeadIds = (parse \\ "coreference") filter { n => (n \\ "@basicPhrases").text == headId } map (n => (n \\ "@id").text)
    System.err.println(s"altheadId: ${altHeadIds}")
    makeClue((parse \\ "predicateArgumentRelation") filter (n => predOrArgIsHead(n, headId, altHeadIds)), parse)
    }
    clues.flatten
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
      extractCluesQ1000(questionXML,parses),
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
      case Question(id,questionType,parses,arrayOfClues,questionText,meta,answers) =>
        <question id={id}>
          {questionText}
          {meta}
          {answers}
          <annotation>
          <questionType>{questionType}</questionType>
          <parses>{for (parse <- parses) yield <parse>
            {parse}
            </parse>}
          </parses>
          <clues>
            {for (clue <- arrayOfClues) yield <clue>
            {clue}
          </clue>}
          </clues>
          </annotation>
        </question>
    }
  }

  def apply(inputXML: Node, parserPath: String): Array[Elem] = {
    val totalQuestions = (inputXML \\ "question").filter(e => (safeMod5((e \ "@id").text) == true)).toArray
    System.err.println(s"Total questions: ${totalQuestions.length}")
    totalQuestions map (q => makeQuestionQ1000(q, parserPath)) map (formatInXML)
    //val targetQuestions = totalQuestions map (q => (q \\"text").text -> questionTypeQ1000(q))
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


  def getNewId: Int = {
    val id = current_id
    current_id += 1
    id
  }
  private var current_id = 0



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


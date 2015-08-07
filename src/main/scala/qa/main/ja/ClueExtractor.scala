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
import scala.xml.Null
import scala.io.Source

case class Clue(headPred: String,
                deps: List[String],
                parse: String)

case class QuestionAndAnnotation(id: String,
                                 questionText: NodeSeq,
                                 meta: NodeSeq,
                                 answers: NodeSeq,
                                 oldAnnotations: NodeSeq
                                )
/**
object XMLLoaderIgnoringDTD extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.setValidating(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}
*/
object ExtractClues {


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
    if ((predIsHead(parseXML, headId) == true) || (argIsHead(parseXML, altHeadIds)==true)) true
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
    System.err.println(s"parseXMLsentence: ${(parseXML \\ "sentence").text}")
    clueMap.map(t=>Clue(findLexemebyId(t._1,parseXML),t._2.map(s=>findLexemebyAltId(s,parseXML)),(parseXML\\"sentence").text.split('\n').map(_.trim.filter(_ >= ' ')).mkString)).toArray
  }


  def extractQuestionAndAnnotation(questionXML: Node):QuestionAndAnnotation = {
    QuestionAndAnnotation((questionXML \ "@id").text,
      questionXML \\ "text",
      questionXML \\ "meta",
      questionXML \\ "answers",
      questionXML \\ "annotations")
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


  def annotateWClues (oldQAndA: QuestionAndAnnotation): QuestionAndAnnotation={
    oldQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, oldAnnotations) => {
        val clues = for (parse <- oldAnnotations \\ "parse") yield {
          val headId =  (((parse \\ "basicPhrase").find(n=>findIdOfGivenHead(n,(meta \\ "B7").text))).getOrElse(<dummy></dummy>)\\"@id").text
          val altHeadIds = (parse \\ "coreference") filter { n => (n \\ "@basicPhrases").text == headId } map (n => (n \\ "@id").text)
          System.err.println(s"altheadId: ${altHeadIds}")
          makeClue((parse \\ "predicateArgumentRelation") filter (n => predOrArgIsHead(n, headId, altHeadIds)), parse)
        }
        System.err.println(s"clues content: ${clues}")
        System.err.println(s"clues flatten length: ${clues.flatten.length}")
        val newAnnotations =
        <annotations>
        {for (annotation <- oldAnnotations \\ "annotation") yield
          {annotation}}
        <annotation type="clue" annotator="ClueExtractor">
          <clues>
            {for (clue <- clues.flatten) yield
              <clue>
              {clue match {
                case Clue(headPred,deps,parse) =>
                  <head>
                    {headPred}
                  </head>
                  <deps>
                    {for (dep <- deps) yield
                      <dep>
                        {dep}
                      </dep>}
                  </deps>
                  <parse>
                    {parse}
                  </parse>
                case _ => ""
                }
              }
              </clue>
            }
          </clues>
        </annotation>
        </annotations>
        QuestionAndAnnotation(id,questionText,meta,answers,newAnnotations)
      }
      case _ => oldQAndA
    }
  }


  /**
 * using meta data to determine clues
 */


  def apply(questionXML: Node): Seq[Elem] ={
  (questionXML \\ "question") map (extractQuestionAndAnnotation) map (annotateWClues) map (formatInXML)
  }
}


object ClueExtractor {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("Usage: scala qa.main.ja.ClueExtractor INPUT_XML OUTPUT_XML")
      System.exit(1)
    }
    val elems = ExtractClues(XMLLoaderIgnoringDTD.loadFile(args(0)))
    XML.save(args(1), <questions>
      {for (elem <- elems) yield {
        elem
      }}
    </questions>, "UTF-8")
  }
}
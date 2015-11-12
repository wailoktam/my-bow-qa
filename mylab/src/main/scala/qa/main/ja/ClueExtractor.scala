package qa.main.ja

import scala.util.matching.Regex
import scala.xml.{ Null, _ }
import java.io._

case class Clue(headPred: String,
                deps: List[String],
                parse: String)

case class QuestionAndAnnotation(id: String,
                                 questionText: NodeSeq,
                                 meta: NodeSeq,
                                 answers: NodeSeq,
                                 oldAnnotations: NodeSeq)
/**
 * object XMLLoaderIgnoringDTD extends XMLLoader[Elem] {
 * override def parser: SAXParser = {
 * val f = javax.xml.parsers.SAXParserFactory.newInstance()
 * f.setNamespaceAware(false)
 * f.setValidating(false)
 * f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
 * f.newSAXParser()
 * }
 * }
 */
object ExtractClues {

  def predIsHead(parseXML: Node, headId: String): Boolean = {
    if ((parseXML \\ "@predicate").text == headId) true
    else false
  }

  def argIsHead(parseXML: Node, altHeadIds: Seq[String]): Boolean = {
    val altHeadIdsRe = altHeadIds.mkString("|").r
    (parseXML \\ "@argument").text match {
      case altHeadRe => true
      case _         => false
    }
  }

  def findIdOfGivenHead(node: Node, givenHead: String): Boolean = {
    val lexemeExtractorRe = """.*?<正規化代表表記:(.+?)/.+>.*""".r
    System.err.println(s"givenHead: ${givenHead}")
    (node \\ "@features").text match {
      case lexemeExtractorRe(lexeme) if (lexeme == givenHead) => true
      case _ => false
    }
  }

  def findIdOfTopic(node: Node): Boolean = {
    val topicIdentifierRe = """.*?(?<!準)主題表現.*""".r
    (node \\ "@features").text match {
      case topicIdentifierRe() => true
      case _                   => false
    }
  }

  def predOrArgIsHead(parseXML: Node, headId: String, altHeadIds: Seq[String]): Boolean = {
    if ((predIsHead(parseXML, headId) == true) || (argIsHead(parseXML, altHeadIds) == true)) true
    else false
  }

  def findLexemebyId(id: String, parseXML: Node): String = {
    val lexemeExtractorRe = """.*?<正規化代表表記:(.+?)/.+>.*""".r
    ((parseXML \\ "basicPhrase").find(n => (n \ "@id").text == id).getOrElse(<dummy></dummy>) \\ "@features").text match {
      case lexemeExtractorRe(lexeme) => {
        System.err.println(s"headPredlexeme: ${lexeme}")
        lexeme
      }
      case _ => {
        System.err.println(s"failedHeadPredLexeme: ${parseXML}")
        System.err.println(s"failedHeadPredLexeme: ${(parseXML \\ "basicPhrase").find(n => (n \ "@id").text == id)}")
        ""
      }
    }
  }

  def findLexemebyAltId(altId: String, parseXML: Node): String = {
    val lexemes = (parseXML \\ "coreference") filter { n => (n \\ "@id").text == altId } map (n => (n \\ "@basicPhrases").text) map (s => findLexemebyId(s, parseXML))
    if (lexemes.length == 1) lexemes.head
    else ""
  }

  def makeClue(semRels: Seq[Node], parseXML: Node, altTopicIDs: Seq[String], bw: BufferedWriter): Array[Clue] = {
    var clueMap = collection.mutable.Map[String, List[String]]()
    bw.write("semRels:" + semRels.mkString + "\n")
    System.err.println(s"semRels: ${semRels.mkString}")
    bw.write("altTopicIDs:" + altTopicIDs.mkString + "\n")
    System.err.println(s"altTopicIDs: ${altTopicIDs.mkString}")
    bw.write("contained number:" + semRels.filter(n => altTopicIDs.contains((n \\ "@argument").text)).length + "\n")
    System.err.println(s"contained number: ${semRels.filter(n => altTopicIDs.contains((n \\ "@argument").text)).length}")
    for (semRel <- semRels.filter(n => altTopicIDs.contains((n \\ "@argument").text))) yield {
      bw.write("altTopicIDs in for loop:" + altTopicIDs.mkString + "\n")
      bw.write("semRel argument in for loop:" + (semRel \\ "@argument").text + "\n")
      System.err.println(s"altTopicIDs in for loop: ${altTopicIDs.mkString}")
      System.err.println(s"semRel argument in for loop: ${(semRel \\ "@argument").text}")
      //    for (semRel <- semRels.filter(n=>n\\"@id" == (semRels.find(n=>altTopicIDs.contains(n \\ "argument"))) \\ "@id").text)) yield {
      if (clueMap.contains((semRel \\ "@predicate").text) == false) {
        clueMap((semRel \\ "@predicate").text) = List((semRel \\ "@argument").text)
      } else clueMap((semRel \\ "@predicate").text) = ((semRel \\ "@argument").text) :: (clueMap((semRel \\ "@predicate").text))
    }

    clueMap.map(t => Clue(findLexemebyId(t._1, parseXML), t._2.map(s => findLexemebyAltId(s, parseXML)), (parseXML \\ "sentence").text.split('\n').map(_.trim.filter(_ >= ' ')).mkString)).toArray
  }

  def extractQuestionAndAnnotation(questionXML: Node): QuestionAndAnnotation = {
    QuestionAndAnnotation((questionXML \ "@id").text,
      questionXML \\ "text",
      questionXML \\ "meta",
      questionXML \\ "answers",
      questionXML \\ "annotations")
  }

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

  def annotateWClues(oldQAndA: QuestionAndAnnotation, bw: BufferedWriter): QuestionAndAnnotation = {
    oldQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, oldAnnotations) => {
        val clues = for (parse <- oldAnnotations \\ "parse") yield {
          val topicId = (parse \\ "basicPhrase").collectFirst { case n if (findIdOfTopic(n)) => (n \\ "@id").text }
          System.err.println(s"basicPhrase: ${(parse \\ "basicPhrase").mkString}")
          //            System.err.println(s"topic: ${topicId.get}")
          //          val headId = (((parse \\ "basicPhrase").find(n => findIdOfGivenHead(n, (meta \\ "B7").text))).getOrElse(<dummy></dummy>) \\ "@id").text
          //          val altTopicIds = (parse \\ "coreference") filter { n => (n \\ "@basicPhrases").text == topicId.get } map (n => (n \\ "@id").text)
          val altTopicIds = (parse \\ "coreference") collect { case n if (topicId != None && (n \\ "@basicPhrases").text == topicId.get) => n } map (n => (n \\ "@id").text)
          bw.write("alTopicId:" + altTopicIds + "\n")
          System.err.println(s"alTopicId: ${altTopicIds}")
          if (altTopicIds.length != 0 && (parse \\ "predicateArgumentRelation").length != 0 && altTopicIds.toSet.intersect((parse \\ "predicateArgumentRelation").map(n => (n \\ "@argument").text).toSet) != Set())
            makeClue((parse \\ "predicateArgumentRelation"), parse, altTopicIds, bw)
          //            makeClue((parse \\ "predicateArgumentRelation") filter (n => predOrArgIsHead(n, headId, altHeadIds)), parse)
          //          else Array(Clue((meta \\ "B7").text, List[String](), (parse \\ "sentence").text))
          else {
            Array(Clue("", List[String](), (parse \\ "sentence").text))
          }
        }
        for (clue <- clues) yield {

          bw.write("every clue content" + clue.mkString + "\n")
          System.err.println(s"every clue content: ${clue.mkString}")

        }
        System.err.println(s"clues content: ${clues}")
        System.err.println(s"clues flatten length: ${clues.flatten.length}")
        val newAnnotations =
          <annotations>
            {
              for (annotation <- oldAnnotations \\ "annotation") yield { annotation }
            }
            <annotation type="clue" annotator="ClueExtractor">
              <clues>
                {
                  for (clue <- clues.flatten) yield <clue>
                                                      {
                                                        clue match {
                                                          case Clue(headPred, deps, parse) =>
                                                            <head>
                                                              { headPred }
                                                            </head>
                                                            <deps>
                                                              {
                                                                for (dep <- deps) yield <dep>
                                                                                          { dep }
                                                                                        </dep>
                                                              }
                                                            </deps>
                                                            <parse>
                                                              { parse }
                                                            </parse>
                                                          case _=> ""
                                                        }
                                                      }
                                                    </clue>
                }
              </clues>
            </annotation>
          </annotations>
        QuestionAndAnnotation(id, questionText, meta, answers, newAnnotations)
      }
      case _ => {
        System.err.println("shit\n")
        oldQAndA
      }
    }
  }

  /**
   * using meta data to determine clues
   */

  def apply(questionXML: Node): Seq[Elem] = {
    val file = new File("bugInClueExtractor")
    val bw = new BufferedWriter(new FileWriter(file))

    (questionXML \\ "question") map (extractQuestionAndAnnotation) map (annotateWClues(_, bw)) map (formatInXML)
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
                        {
                          for (elem <- elems) yield {
                            elem
                          }
                        }
                      </questions>, "UTF-8")
  }
}
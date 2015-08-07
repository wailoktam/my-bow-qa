package qa.main.ja
import scala.xml._
import scala.xml.factory.XMLLoader


object BaselineAnswering {

  def annotateWResponses (oldQAndA: QuestionAndAnnotation): QuestionAndAnnotation = {
      oldQAndA match {
        case QuestionAndAnnotation(id,questionText,meta,answers,oldAnnotations) => {
          val newAnnotations =
            <annotations>
              {for (annotation <- oldAnnotations \\ "annotation") yield
                {annotation}
              }
              <annotation type="response" annotator="AnswerTitle">
                <responses>
                  {for (doc <- oldAnnotations \\ "doc") yield
                      <response>
                        {(doc \\ "dtitle").text}
                      </response>
                  }
                  </responses>
              </annotation>
            </annotations>
        QuestionAndAnnotation(id,questionText,meta,answers,newAnnotations)
        }
        case _ => oldQAndA
      }
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



  def apply (xmlWDocs: Node): Seq[Elem] = {
    (xmlWDocs \\ "question") map (extractQuestionAndAnnotation) map (annotateWResponses) map (formatInXML)
  }
}


object AnswerTitle {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("Usage: scala qa.main.ja.AnswerTitle INPUT_XML OUTPUT_XML")
      System.exit(1)
    }
    val elems = BaselineAnswering(XMLLoaderIgnoringDTD.loadFile(args(0)))
    XML.save(args(1), <questions>
      {for (elem <- elems) yield {
        elem
      }}
    </questions>, "UTF-8")
  }
}

/**
 * old code for reference
 * case class QuestionAsTextPairs(id: String, // 回答欄ID
                               questionType: QuestionType.Value, // 問題文のタイプ
                               textPairs: Array[(String, String)] // premise/hypotheis のペア
                               ) {
  def toXML(): Node = {
    val textPairsXML = textPairs map {
      case (premise, hypothesis) =>
        <pair>
          <t>
            { premise }
          </t>
          <h>
            { hypothesis }
          </h>
        </pair>
    }
    <questionAsTextPairs id={ id } type={ questionType.toString }>
      { textPairsXML }
    </questionAsTextPairs>
  }
}
 * val newAnnotations =
        <annotations>
          {for (annotation <- oldAnnotations) yield
        {annotation}}
          <annotation type="doc" annotator="SearchDocument">
            <docs>
              {for (scoreWDoc <- index_searcher.search(query, maxHits).scoreDocs) yield
            <doc>
              <id>
                {index_searcher.doc(scoreWDoc.doc).get("id")}
              </id>
              <title>
                {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "title").text}
              </title>
              <text>
                {(document_map.get(index_searcher.doc(scoreWDoc.doc).get("id")).get \ "text").text}
              </text>
            </doc>}
        </docs>
        </annotation>
        </annotations>
        QuestionAndAnnotation(id,questionText,meta,answers,newAnnotations)
        }
      case _ => oldQAndA
    }
  }
question match {
case Question(id, questionType, parses, questionText, meta, answers) =>
var hitsFrSameClue = new Array[SearchResult](1)
var hitsFrSameClueWMaxScore = new Array[SearchResult](1)
var counter: Int = 0

for (clue <- arrayOfClues) yield {
          hitsFrSameClue = questionType match {
            case QuestionTypeQ1000.where =>
              clue match {
                case Clue(headPred, deps, text) =>
                  //        val headRegex="("+headPred+")".r
                  //        val depRegex="(.*)"+("+deps.mkString("|")+")".r
                  search(text, 1)
                case _ => new Array[SearchResult](0)
              }
            case _ => new Array[SearchResult](0)
          }
          if (counter == 0) {
            hitsFrSameClueWMaxScore = hitsFrSameClue
          } else {
            if (sumOfScores(hitsFrSameClue) > sumOfScores(hitsFrSameClueWMaxScore)) {
              hitsFrSameClueWMaxScore = hitsFrSameClue
            }
          }
          counter += 1
        }

<question id={ id }>
{ questionText }
{ answers }
{ meta }
<responses annotator="baseline">
{
for (hit <- hitsFrSameClueWMaxScore) yield <response>{ makeResponse(hit) }</response>
}
</responses>
</question>

case _ => <question></question>
}
}

}

  def sumOfScores (searches: Array[SearchResult] ): Double = {
var increment: Double = 0
for (search <- searches) yield {
search match {
case SearchResult (id, title, text, score) => increment += score
case _ => increment
}
}
increment
}
*/
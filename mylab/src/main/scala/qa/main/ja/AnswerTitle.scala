//this is the last step of the BOW QA pipeline
//it provides the title of the highest ranked doc as the answer

package qa.main.ja
import scala.xml._

object BaselineAnswering {

  def annotateWResponses(oldQAndA: QuestionAndAnnotation): QuestionAndAnnotation = {
    oldQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, oldAnnotations) => {
        val newAnnotations =
          <annotations>
            {
              for (annotation <- oldAnnotations \\ "annotation") yield { annotation }
            }
            <annotation type="response" annotator="AnswerTitle">
              <responses>
                {
                  for (doc <- oldAnnotations \\ "doc") yield <response>{ (doc \\ "dtitle").text.trim }</response>
                }
              </responses>
            </annotation>
          </annotations>
        QuestionAndAnnotation(id, questionText, meta, answers, newAnnotations)
      }
      case _ => oldQAndA
    }
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

  def apply(xmlWDocs: Node): Seq[Elem] = {
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
                        {
                          for (elem <- elems) yield {
                            elem
                          }
                        }
                      </questions>, "UTF-8")
  }
}


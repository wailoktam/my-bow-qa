package qa.main.ja

import java.io.File

import org.apache.lucene.index.{DirectoryReader, IndexReader}
import PrepareTrain._
import org.apache.lucene.store.FSDirectory
import scala.xml._

object RealAnswering {





  def annotateWSentences(oldQAndA: QuestionAndAnnotation, indexDir: FSDirectory): QuestionAndAnnotation = {
    val index_reader = DirectoryReader.open(indexDir)
    oldQAndA match {
      case QuestionAndAnnotation(id, questionText, meta, answers, oldAnnotations) => {
        val newAnnotations =
          <annotations>
            {
              for (annotation <- oldAnnotations \\ "annotation") yield { annotation }
            }
            <annotation type="sentence" annotator="AnswerSentence">
              <sentences>
                {
                  for (doc <- oldAnnotations \\ "doc") yield <sentence>{ ((doc \\ "dtext").text.trim().split("。")).maxBy(PrepareTrainMain.proximity(_, questionText.text, index_reader)) }</sentence>
                }
              </sentences>
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

  def apply(xmlWDocs: Node, indexDir: FSDirectory): Seq[Elem] = {
    (xmlWDocs \\ "question") map (extractQuestionAndAnnotation) map (annotateWSentences(_,indexDir)) map (formatInXML)
  }
}

object AnswerSentence {
  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      System.err.println("Usage: scala qa.main.ja.AnswerSentence INPUT_XML OUTPUT_XML INDEX_DIR")
      System.exit(1)
    }

    val elems = RealAnswering(XMLLoaderIgnoringDTD.loadFile(args(0)),FSDirectory.open(new File(args(2))))
    XML.save(args(1), <questions>
                        {
                          for (elem <- elems) yield {
                            elem
                          }
                        }
                      </questions>, "UTF-8")
  }
}



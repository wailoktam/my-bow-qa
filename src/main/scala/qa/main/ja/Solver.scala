package qa.main.ja
import scala.xml._

/**
 * Questions represented as t/h pairs
 * @param questionType
 */
/*
case class QuestionAsTextPairs(id: String, // 回答欄ID
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
*/

/**
 * Question Answering System
 */
class Solver(val parser: JiggParser, val search: SearchDocument, val scoreThreshold: Double) {

  /**
   * true/false の列を、数字（選択肢番号）に変換する
   * TODO: RTEの true/false から選択肢へのマッピングをちゃんとやる（現在は「正-正、正-誤、誤-正、誤-誤」を仮定）
   * @param t_or_f
   * @param num
   * @return
   *
   *
   * def mapTrueOrFalse(t_or_f: List[Boolean], num: Int = 0): Int = {
   * t_or_f match {
   * case Nil => num
   * case head :: tail =>
   * mapTrueOrFalse(tail, num * 2 + (if (head) 0 else 1))
   * }
   * }
   *
   * /**
   * Return a score to judge whether choice is true or not
   * */
   * def confidenceScore(hypothesis: String): Double = {
   * // search textbooks and create t/h pairs
   * System.err.println("-------------------------------------------------------------------------")
   * System.err.println("Search textbooks for premise")
   * val searchResults = search(hypothesis)
   * val searchResultsTexts = searchResults.sortBy(_.id.toInt).map(_.text)
   * val premise = searchResultsTexts.mkString("\n")
   * System.err.println(s"hypothesis: $hypothesis")
   * System.err.println(s"premise: $premise")
   * // do something here
   * return 0.0
   * }
   */

  def sumOfScores(searches: Array[SearchResult]): Double = {
    var increment: Double = 0
    for (search <- searches) yield {
      search match {
        case SearchResult(id, title, text, score) => increment += score
        case _                                    => increment
      }
    }
    increment
  }

  def makeResponse(searchResult: SearchResult): String = {
    searchResult match {
      case SearchResult(id, title, text, score) => title
      case _                                    => ""
    }

  }

  def apply(question: Question): Elem = {
    question match {
      case Question(id, questionType, parses, arrayOfClues, questionText, meta, answers) =>
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

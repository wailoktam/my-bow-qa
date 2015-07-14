package qa.main.ja

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
class Solver(val parser: KNP, val search: SearchDocument, val scoreThreshold: Double) {

  /**
   * true/false の列を、数字（選択肢番号）に変換する
   * TODO: RTEの true/false から選択肢へのマッピングをちゃんとやる（現在は「正-正、正-誤、誤-正、誤-誤」を仮定）
   * @param t_or_f
   * @param num
   * @return
   */
  def mapTrueOrFalse(t_or_f: List[Boolean], num: Int = 0): Int = {
    t_or_f match {
      case Nil => num
      case head :: tail =>
        mapTrueOrFalse(tail, num * 2 + (if (head) 0 else 1))
    }
  }

  /**
   * Return a score to judge whether choice is true or not
   */
  def confidenceScore(hypothesis: String): Double = {
    // search textbooks and create t/h pairs
    System.err.println("-------------------------------------------------------------------------")
    System.err.println("Search textbooks for premise")
    val searchResults = search(hypothesis)
    val searchResultsTexts = searchResults.sortBy(_.id.toInt).map(_.text)
    val premise = searchResultsTexts.mkString("\n")
    System.err.println(s"hypothesis: $hypothesis")
    System.err.println(s"premise: $premise")
    // do something here
    return 0.0
  }

  def apply(question: (Array[Clues], QuestionTypeQ1000.Value)): Array[Unit] = {
    // run Solver and obtain score
    val questionClues = question._1
    val questionType = question._2
    //   val choices = question.choices
    //   System.err.println("===========================================================")
    //   System.err.println(s"text: $questionClue")
    // output an answer based on the question type
    for (questionClue <- questionClues) yield {
      val finalAnswer = questionType match {
        case QuestionTypeQ1000.where =>
          questionClue match {
            case Clues(headPred, deps, text) =>
              //        val headRegex="("+headPred+")".r
              //        val depRegex="(.*)"+("+deps.mkString("|")+")".r
              val results = search(text, 1)
              for (result <- results) {
                println("%s %s: %f".format(result.id, result.title, result.score))
              }
              results
          }
        //search test = do nothing
      }
    }
  }
}
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

  def apply(question: Question): String = {
    // run Solver and obtain score
    val anscolID = question.id
    val questionType = question.questionType
    val choices = question.choices
    System.err.println("===========================================================")
    System.err.println(s"answerID: $anscolID type: $questionType")
    var numErrors = 0
    val solverScores = for (choice <- choices) yield {
      System.err.println("==========")
      System.err.println(s"choice: $choice")
      val score =
        try {
          confidenceScore(choice)
        } catch {
          // fail safe for errors in parsing and inference
          case e: Throwable =>
            e.printStackTrace(System.err)
            numErrors += 1
            0.0
        }
      score
    }
    System.err.println("==========")
    System.err.println(s"scores: ${
      solverScores.toList
    }")

    // output an answer based on the question type
    val finalAnswer = questionType match {
      case QuestionType.sentence_true =>
        // 正しい文を選ぶ -> proof score が一番大きい選択肢を選ぶ
        System.err.println("Select a choice with max proof score")
        val max_score_id = solverScores.zipWithIndex.maxBy(_._1)._2
        (max_score_id + 1).toString
      case QuestionType.sentence_false =>
        // 誤っている文を選ぶ -> proof score が一番小さい選択肢を選ぶ
        System.err.println("Select a choice with min proof score")
        val min_score_id = solverScores.zipWithIndex.minBy(_._1)._2
        (min_score_id + 1).toString
      case QuestionType.true_false =>
        // 正誤の組み合わせを選ぶ -> threshold を境に 正・誤 を決める
        val true_or_false = solverScores map (_ >= scoreThreshold)
        System.err.println("Select a choice with appropriate true-false combinations")
        val answer = mapTrueOrFalse(true_or_false.toList)
        (answer + 1).toString
    }

    finalAnswer
  }
}

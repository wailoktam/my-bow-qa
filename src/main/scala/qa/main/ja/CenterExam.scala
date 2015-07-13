// Answer questions of Center Exam
// Assuming the XML format of Torobo project

package qa.main.ja

import scala.collection.mutable
import scala.xml.factory.XMLLoader
import scala.xml._

/**
 * Reading XML files without reading DTD
 */
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

/**
 * Correct answer table
 * @param answers answer column ID -> (answer, score)
 */
case class AnswerTable(answers: Map[String, (String, Int)] = Map.empty[String, (String, Int)]) {
  def getAnswer(id: String): Option[String] = answers.get(id).map(_._1)
  def getScore(id: String): Option[Int] = answers.get(id).map(_._2)
  def isEmpty: Boolean = answers.isEmpty
}

/**
 * Type of a question
 */
object QuestionType extends Enumeration {
  val sentence_true, // choose a true statement
  sentence_false, // choose a false statement
  true_false, // choose a combination of true/false
  other // other (not answered by this system)
  = Value
}

object QuestionTypeQ1000 extends Enumeration {
  val how_many, // choose a true statement
  when, // choose a false statement
  where, // choose a combination of true/false
  what,
  who,
  what_kind_of,
  other// other (not answered by this system)
  = Value
}

/**
 * Question extracted from q1000
 * といえば、は、何という　what
 *
 */



/**
 * Question extracted from Center Test
 */
case class Clues(headPred:String,
                 deps: Array[String],
                 text: String
                  )

case class Question(id: String, // 回答欄ID
                    questionType: QuestionType.Value, // 問題文のタイプ
                    clues: Array[String], // 問題文中の重要語
                    times: Array[String], // 問題文中の時間表現
                    choices: Array[String] // 選択肢から抽出した文
                    ) {
  def toXML(): Node = {
    val cluesXML = clues map (c => <clue>{ c }</clue>)
    val timesXML = times map (t => <time>{ t }</time>)
    val choicesXML = choices map (s => <choices>{ s }</choices>)
    <question id={ id } questionType={ questionType.toString }>
      { cluesXML }
      { timesXML }
      { choicesXML }
    </question>
  }
}

/**
 * Extract questions and choices and clue expressions
 */
object ExtractQuestions {

  /**
   * 試験問題 XML から下線部テキストを抽出し、id -> テキスト の Map を作る
   * @param root 試験問題 XML
   * @return id -> テキストの Map
   */
  def extractUnderlinedTexts(root: Node): Map[String, String] = {
    val utexts =
      for (utext_node <- (root \\ "uText")) yield {
        val id = (utext_node \ "@id").text
        val text = utext_node.child.collect { case Text(t) => t }.mkString("") // uText の下には<label>などがあるので、テキストノードだけ抽出
        id -> text
      }
    //println(utexts)
    utexts.toMap
  }

  /**
   * 質問タイプを求める
   * TODO: 「誤っているものを選ぶ」問題の認識を改良する（現在は単純なパターンマッチ）
   */
  def questionType(question: Node): QuestionType.Value = {
    val true_false_regex = """.*(symbol-TF).*""".r // 正誤選択問題の answer_type
    (question \ "@answer_type").text match {
      case "sentence" =>
        val instruction_text = (question \ "instruction").text.replaceAll("\n+", "")
        if (instruction_text.matches(".*(誤っ|誤り|適当でない|適切でない).*"))
          QuestionType.sentence_false
        else
          QuestionType.sentence_true
      case true_false_regex(_) => QuestionType.true_false
      case _                   => QuestionType.other
    }
  }

  /**
   * 下線部を参照している箇所を、参照先のテキストで置き換える
   * @param node
   * @return
   */
  def replaceUnderline(node: Node, underline_texts: Map[String, String]): String = {
    val elems =
      for (elem <- node.child) yield {
        println("elem"+elem)
        elem match {
          case ref @ <ref>
                       { _ }
                     </ref> if ((ref \ "@target").text(0) == 'U') =>
            // 下線部を参照している
            val id = (ref \ "@target").text
            val utext = underline_texts.getOrElse(id, "")
            println("utext"+utext)
            println("id"+id)
            utext
          case e =>
            e.text
            println("e"+e.text)
        }
      }
    elems.mkString("").replace("下線部", "").trim
  }

  /**
   * 問題文からキーワード（「新羅」について...）を抽出する
   * @param instruction
   * @return
   */
  def extractClues(instruction: String): Array[String] = {
    val phrases = instruction.replaceAll("""[（(].*?[)）]""", "").split("[，．、。]") // かっこは邪魔なので消す
    val clue_pattern_regex = "^(.+)(に起こった出来事|の出来事|で起こった出来事|について).*".r
    val clues =
      phrases collect {
        case clue_pattern_regex(clue, _) => clue
      }
    clues.toArray
  }




  /**
   * 問題文から時間表現を抽出する
   * @param instruction
   * @return
   */
  def extractTime(instruction: String): Array[String] = {
    val phrases = instruction.replaceAll("""[（(].*?[)）]""", "").split("[，．、。]") // かっこは邪魔なので消す
    val time_pattern_regex = "^(.+)(にかけて|の時期|の時代).*".r
    val time_pattern_regex2 = "^(.+時代).*".r
    val times =
      phrases collect {
        case time_pattern_regex(time, _) => time
      }
    val times2 =
      if (times.nonEmpty) times
      else {
        phrases collect {
          case time_pattern_regex2(time) => time
        }
      }
    times2.toArray
  }

  /**
   * 問題文から証明すべき文を抽出する
   * TODO: add keywords from instruction sentence
   * @param qtype
   * @param question
   * @return
   */
  def extractStatements(qtype: QuestionType.Value, question: Node): Array[String] = {
    val texts =
      qtype match {
        case QuestionType.sentence_true | QuestionType.sentence_false =>
          val choices = question \ "choices" \ "choice"
          require(choices.nonEmpty)
          choices.map(_.text).toArray
        case QuestionType.true_false =>
          val texts = question \ "data" \ "lText"
          require(texts.nonEmpty)
          texts.map(_.text).toArray
      }
    texts.map(_.trim.replaceAll("""[●■①-⑩➊-➏a-dａ-ｄ]""", ""))
  }

  /**
   * 必要なら time あるいは clue を足したり、不要な表現を消す
   * @param statement
   * @param times
   * @param clues
   * @return
   */
  private def addClues(statement: String, times: Array[String], clues: Array[String]): String = {
    val hypo =
      if (times.nonEmpty) {
        // 時間表現は足す
        times.mkString("", "，", "には，") + statement
      } else if (statement.matches("""^.*(この.*?)(は|が|を|に（).*""")) {
        // 「この〜」を clue で置き換える
        statement.replaceFirst("""(この.{1,4}?)(は|が|を|に（)""", clues.mkString("，") + "$2")
      } else if (statement.matches(""".*(は，|が，).*""") || statement.take(15).matches(""".*(は|が).*""")) {
        // 主語があるときは clue を足さない
        statement
      } else {
        clues.mkString("", "，", "は，") + statement
      }
    hypo.replaceAll("""[(（].*?[）)]""", "") // かっこは消す
  }

  /**
   * センター試験 XML から、含意関係認識で解くべき問題を抽出し、問題文から重要語や時間表現などを抽出する
   * @param examXML
   * @return
   */
  def apply(examXML: Node): Array[Question] = {
    // extract target questions (i.e. question type != other)
    val totalQuestions = (examXML \\ "question").filter(e => (e \ "@minimal").text == "yes").toArray
    System.err.println(s"Total questions: ${totalQuestions.length}")
    val targetQuestions = totalQuestions map (q => q -> questionType(q)) filter (_._2 != QuestionType.other)
    System.err.println(s"Target questions: ${targetQuestions.length}")

    // extract underlined texts (to be used to extract clues from instruction texts)
    val underlinedTexts = extractUnderlinedTexts(examXML)

    // obtain clues and statements
    val preprocessedQuestions = targetQuestions map {
      case (question, questionType) =>
        val id = (question \ "@id").text
        val anscol = (question \ "@anscol").text
        val instruction_text = replaceUnderline((question \ "instruction").head, underlinedTexts)
        System.err.println("===========================================================")
        System.err.println(s"ID: $id answerID: $anscol type: $questionType")
        System.err.println(instruction_text)
        //System.err.println(question.text)
        // extract clues and statements
        val times = extractTime(instruction_text)
        val clues = extractClues(instruction_text)
        val statements = extractStatements(questionType, question) map { addClues(_, times, clues) }
        System.err.println("time: " + times.mkString(" ") + " clues: " + clues.mkString(" "))
        System.err.println("statements:")
        statements foreach System.err.println
        Question(anscol, questionType, clues, times, statements)
    }
    preprocessedQuestions
  }
}

object ExtractQuestionsQ1000 {

    def questionTypeQ1000(question: Node): QuestionTypeQ1000.Value = {
      System.err.println(s"Column5: ${(question \\ "B1").text}")
      val whereRe= """(どこ)""".r
      val whenRe= """(いつ)""".r
      val howManyRe= """(いくつ|いくら|どれぐらい)""".r
      val whatKindOfRe= """(どんな)""".r
      val isRe= """(は)""".r
      val whatRe= """(なに+)""".r
      val whatNotUnitRe= """(なに\+ひらがな)""".r
      val howManyUnitRe= """(なに\+漢字)""".r
      val whichYearRe= """(何年)""".r
      val howManyPplRe= """(何人)""".r
      val whoRe= """(誰)""".r
      val dimensionRe= """(数|体積|長さ|温度|頻度)""".r
      val timeExprRe= """(日付表現)""".r

      (question \\ "B1").text match {
        case whereRe(questionWord) => QuestionTypeQ1000.where
        case howManyRe(questionWord) => QuestionTypeQ1000.how_many
        case whenRe(questionWord) => QuestionTypeQ1000.when
        case whatKindOfRe(questionWord) => QuestionTypeQ1000.what_kind_of
        case isRe(questionWord) => QuestionTypeQ1000.what
        case whatRe(questionWord) => QuestionTypeQ1000.what
        case whatNotUnitRe(questionWord) => QuestionTypeQ1000.what
        case howManyUnitRe(questionWord) =>
          if (dimensionRe.findFirstMatchIn((question \\"D3").text)!=None)
            QuestionTypeQ1000.how_many
          else
            QuestionTypeQ1000.what
        case whichYearRe(questionWord) =>
          if (timeExprRe.findFirstMatchIn((question \\ "D3").text)!=None)
            QuestionTypeQ1000.when
          else
            QuestionTypeQ1000.how_many
        case howManyPplRe(questionWord) => QuestionTypeQ1000.how_many
        case whoRe(questionWord) => QuestionTypeQ1000.who
        case _                   => QuestionTypeQ1000.other


      }


  }

  def extractCluesQ1000(text: String, headWord: String): Clues = {
    val knp = new KNP("juman", "knp")
    val parses = knp.parse(text)
    var entityMap = collection.mutable.Map[String,String]()
    var depTemp = mutable.ArrayBuffer[String]()
    for (node<-parses) yield {
      node match {
        case 基本句ノード(token, depRel, argRels) => token match {
          case 基本句
            (tokenID, lemma, pos, coarsePos, voice, ne, negated, features, surfaceString, entityID) => {
            if ((lemma == headWord) && (argRels.length != 0)) {
              for (argRel <- argRels) yield {
                argRel match {
                  case 述語項関係(role, entityID) => depTemp += entityMap(entityID)
                }
              }
            }
            else {
              entityMap += (entityID -> lemma)
            }
          }
        }
      }
    }
    Clues(headWord,depTemp.toArray,text)
  }


  def safeMod5(stringIn: String): Boolean ={
    var boolOut=true
    val idRe= """LC-ECQA2002-(\d+)-01""".r

    stringIn match {
      case idRe(id) =>
        System.err.println(s"mod5: ${id.toInt%5}, ${id.toInt}")
        if (id.toInt%5 == 0)
          boolOut=false
    }
    boolOut
  }
   /**
   * センター試験 XML から、含意関係認識で解くべき問題を抽出し、問題文から重要語や時間表現などを抽出する
   * @param examXML
   * @return
   */
  def apply(examXML: Node): Array[(Clues,QuestionTypeQ1000.Value)] = {
    // extract target questions (i.e. question type != other)

    val totalQuestions = (examXML \\ "question").filter(e => (safeMod5((e \"@id").text) == true)).toArray
    System.err.println(s"Total questions: ${totalQuestions.length}")
    val targetQuestions = totalQuestions map (q => extractCluesQ1000((q \\"text").text, (q \\ "B7").text) -> questionTypeQ1000(q)) filter (_._2 != QuestionTypeQ1000.other)
    //val targetQuestions = totalQuestions map (q => (q \\"text").text -> questionTypeQ1000(q))
    System.err.println(s"Target questions: ${targetQuestions.length}, ${targetQuestions{0}.toString()}")
    targetQuestions
  }
}

/**
 * Answer questions of Center Exam
 * Focused on questions that can be answered by RTE
 */
object CenterExam {

  /**
   * Parser; currently we use Juman and KNP
   * TODO: tuning the parser and/or incorporating new resources (dictionary etc.)
   */
  //lazy val parser = new KNP("juman -r jumandic/jumanrc", "knp")
  lazy val parser = new KNP("juman", "knp")

  /**
   * Resources
   */
  //lazy val jaSimilarity = new JaSimilarity("ja/WordVectors/rite2-ja-wiki.vec300.cdb")
  //UserDic.importSynonyms(getClass.getClassLoader.getResourceAsStream("ja/EventOntology/synonym.txt"))

  /**
   * 知識源としてつかうデータ
   * TODO: specify textbook files in command line
   */
  val TEXTBOOKS = Array("input/textbooks/rite2-ja-textbook.xml",
    "input/textbooks/riteval-ja-textbook2.xml","input/wiki/wiki_00")

  // パラメータ
  // 教科書から検索してくるパラグラフの数
  var maxSearchResults = 5

  // 推論前に入れる知識でつかう threshold
  //var wordSimilarityThreshold = 0.7

  // 正誤の組み合わせ判定問題で、正誤判定に用いる score のしきい値
  var scoreThreshold = 0.6

  /**
   * メインプログラム
   * @param args
   */
  def main(args: Array[String]): Unit = {
    if (args.length < 4) {
      System.err.println("Usage: scala qa.main.ja.CenterExam MAX_SEARCH_RESULTS PROOF_THRESHOLD EXAM_XML OUTPUT_XML [ANSWER_XML]")
      System.exit(1)
    }

    maxSearchResults = args(0).toInt
    //wordSimilarityThreshold = args(1).toDouble
    scoreThreshold = args(1).toDouble
    val exam_xml_name = args(2)
    val output_xml_name = args(3)
    val answer_xml_name = if (args.length == 5) args(4) else null

    System.err.println(s"Max search results: $maxSearchResults")
    //System.err.println(s"Word similarity threshold: $wordSimilarityThreshold")
    System.err.println(s"Score threshold: $scoreThreshold")
    System.err.println(s"Exam file: $exam_xml_name")
    System.err.println(s"Output file: $output_xml_name")
    if (answer_xml_name != null)
      System.err.println(s"Answer table file: $answer_xml_name")

    // read answer table
    val answerTable =
      if (answer_xml_name == null) new AnswerTable
      else {
        // read correct answers
        val answer_xml = XMLLoaderIgnoringDTD.loadFile(answer_xml_name)
        val answers =
          for (data <- answer_xml \\ "data") yield {
            val anscol = (data \ "anscolumn_ID").text
            val ans = (data \ "answer").text
            val score = (data \ "score").text.toInt
            (anscol, (ans, score))
          }
        new AnswerTable(answers.toMap)
      }

    // create index of textbooks
    System.err.println("Creating index of textbooks")
    //SearchDocument.makeIndexOnFile(target_files, index_dir, cdb_file)
    val (index, documents) = SearchDocument.makeIndexOnMemory(TEXTBOOKS)
    System.err.println("done")
    // instance for document search
    val search = new SearchDocument(index, documents, maxSearchResults)

    // extract questions
    System.err.println("-------------------------------------------------------------------------")
    System.err.println("Preprocessing target questions")
    val examXML = XMLLoaderIgnoringDTD.loadFile(exam_xml_name)
    val questions = ExtractQuestionsQ1000(examXML)
    // Run Solver and obtain final answers
    System.err.println("-------------------------------------------------------------------------")
    System.err.println("Run QA solver")
    val solver = new Solver(parser, search, scoreThreshold)
    val finalAnswers = for (question <- questions) yield {
      val finalAnswer = solver(question)

    }

    // output all the answers to the XML file
    /**
    val output_elems =
      for ((anscol, answer) <- finalAnswers) yield {
        <data>
          <anscolumn_ID>{ anscol }</anscolumn_ID>
          <answer>{ answer }</answer>
        </data>
      }
    XML.save(output_xml_name, <answerTable>{ output_elems }</answerTable>)
    */
    // compute scores if answer table is given
    /**
    if (!answerTable.isEmpty) {
      val scores = // list of (score of a question, obtained score)
        for ((anscol, answer) <- finalAnswers) yield {
          val score = answerTable.getScore(anscol).get
          if (answerTable.getAnswer(anscol).get == answer)
            (score, score)
          else
            (score, 0)
        }
      val (total_scores, obtained_scores) = scores.unzip
      System.err.println(s"Total questions: ${total_scores.length}")
      System.err.println(s"Correct answers: ${obtained_scores.filter(_ != 0).length}")
      System.err.println(s"Score: ${obtained_scores.sum}/${total_scores.sum}")
      //System.err.println(s"Errors: $numErrors")
    }
      */
  }
}

//check usage
//unzip
//SearchDocument.makeIndexOnMemory
//apply
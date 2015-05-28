// Wrapper for Juman/KNP
// KNP.parse parses a text into KNP parse trees （KNP解析木)

package qa.main.ja

import java.io.{ OutputStreamWriter, BufferedWriter, InputStreamReader, BufferedReader }
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer
import com.ibm.icu.text.Transliterator
import scala.io.Source

// Token -> DCS node
case class 基本句(var tokenID: String,
               var lemma: String, var pos: String, var coarsePos: String,
               var voice: String, var ne: String, var negated: Boolean, var features: Array[String], surfaceString: String,
               var entityID: String)
// Dependency relation -> DCS edge
case class 係り受け(var marker: String, var headID: String)
// Argument relation -> DCS edge
case class 述語項関係(var role: String, var entityID: String)
// Sentence = Tokens + DepRel + ArgRels
//case class 基本句ノード(var token: 基本句, var depRel: 係り受け, var argRels: Array[述語項関係], var local_node_end_dict: collection.mutable.Map[Int,Int], var surface_string: String, var space: Int, var token_id: Int)
case class 基本句ノード(var token: 基本句, var depRel: 係り受け, var argRels: Array[述語項関係])

case class KNP解析木(tokens: Array[基本句ノード]) {
  val transliterator = Transliterator.getInstance("Halfwidth-Fullwidth")
  def prettyPrint: String = {
    var head_deps_dict = collection.mutable.Map[Int, collection.mutable.Buffer[Int]]()
    var node_end_dict = collection.mutable.Map[Int, Int]()
    var head_list = collection.mutable.Buffer[Int]()
    var node_loc_dict = collection.mutable.Map[Int, Int]()
    var node_space_dict = collection.mutable.Map[Int, Int]()
    var node_end_dict_feat = collection.mutable.Map[Int, Int]()
    var node_loc_dict_feat = collection.mutable.Map[Int, Int]()
    var node_space_dict_feat = collection.mutable.Map[Int, Int]()
    var space_accumulator = 0
    var space_accumulator_feat = 0
    tokens.foreach((token) => {
      head_list += token.depRel.headID.toInt
      if (head_deps_dict contains token.depRel.headID.toInt.toInt) {
        head_deps_dict(token.depRel.headID.toInt.toInt) += token.token.tokenID.toInt
      } else {
        head_deps_dict += (token.depRel.headID.toInt -> collection.mutable.Buffer(token.token.tokenID.toInt))
      }
      if (head_list contains token.token.tokenID.toInt) {
        space_accumulator = {
          head_deps_dict(token.token.tokenID.toInt) map (node_loc_dict(_))
        }.max
        space_accumulator_feat = {
          head_deps_dict(token.token.tokenID.toInt) map (node_loc_dict_feat(_))
        }.max
        for (token <- head_deps_dict(token.token.tokenID.toInt)) yield {
          node_end_dict(token) = space_accumulator
          node_end_dict_feat(token) = space_accumulator_feat
        }
      } else {
        space_accumulator = 0
        space_accumulator_feat = 0
      }
      node_space_dict += (token.token.tokenID.toInt -> space_accumulator)
      node_space_dict_feat += (token.token.tokenID.toInt -> space_accumulator_feat)
      //        var space = space_accumulator
      //        var local_node_end_dict = node_end_dict
      //      commented out for adding argrel
      node_loc_dict_feat += (token.token.tokenID.toInt -> {
        space_accumulator_feat + token.token.lemma.length() + {
          Array(token.token.pos, token.token.coarsePos, token.token.voice, token.token.ne, token.token.negated) ++: token.token.features.toArray
        }.map { feat => "（" + "%s".format(feat) + "）" }.mkString.length
      })
      node_loc_dict += (token.token.tokenID.toInt -> {
        1 + token.token.entityID.length + space_accumulator + token.token.surfaceString.length() + token.argRels.map { argrel => "（" + "%s".format(argrel.role) + "%s".format(argrel.entityID) + "）" }.mkString.length
      })
    })
    val sorted_node_end_dict = ListMap(node_end_dict.toSeq.sortBy(_._1): _*)
    var node_cross_dict = collection.mutable.Map[Int, collection.mutable.Buffer[Int]]()
    val sorted_node_end_dict_feat = ListMap(node_end_dict_feat.toSeq.sortBy(_._1): _*)
    var node_cross_dict_feat = collection.mutable.Map[Int, collection.mutable.Buffer[Int]]()
    for ((key, value) <- sorted_node_end_dict) yield {
      var cross_list = collection.mutable.Buffer[Int]()
      var prev_key = key - 1
      // not first row but head position before position of head of previous row
      while (prev_key > 0 && !(sorted_node_end_dict.contains(prev_key))) {
        prev_key -= 1
      }
      if ((key > 0) && (sorted_node_end_dict.contains(prev_key)) && value < sorted_node_end_dict(prev_key)) {
        cross_list += sorted_node_end_dict(prev_key)
        node_cross_dict += (key -> cross_list)
      }

      if (node_cross_dict contains { prev_key }) {
        for (prev_val <- node_cross_dict(prev_key)) yield {
          if (value < prev_val) {
            cross_list += prev_val
            node_cross_dict += (key -> cross_list)
          }
        }
      }
    }
    for ((key, value) <- sorted_node_end_dict_feat) yield {
      var cross_list = collection.mutable.Buffer[Int]()
      var prev_key = key - 1
      // not first row but head position before position of head of previous row
      while (prev_key > 0 && !(sorted_node_end_dict_feat.contains(prev_key))) {
        prev_key -= 1
      }
      if ((key > 0) && (sorted_node_end_dict_feat.contains(prev_key)) && value < sorted_node_end_dict_feat(prev_key)) {
        cross_list += sorted_node_end_dict_feat(prev_key)
        node_cross_dict_feat += (key -> cross_list)
      }

      if (node_cross_dict_feat contains { prev_key }) {
        for (prev_val <- node_cross_dict_feat(prev_key)) yield {
          if (value < prev_val) {
            cross_list += prev_val
            node_cross_dict_feat += (key -> cross_list)
          }
        }
      }
    }
    val token_tree =
      tokens map {
        token_node =>
          if (sorted_node_end_dict contains token_node.token.tokenID.toInt) {
            if (node_cross_dict contains token_node.token.tokenID.toInt) {
              val cross_str = new StringBuilder
              var cross_ctr: Int = 0
              var cross_store: Int = 0

              for (cross <- node_cross_dict { token_node.token.tokenID.toInt }) yield {
                cross_str ++= "　" * { node_cross_dict(token_node.token.tokenID.toInt)(cross_ctr) - sorted_node_end_dict(token_node.token.tokenID.toInt) - cross_store - 1 } + "｜"
                cross_store += node_cross_dict(token_node.token.tokenID.toInt)(cross_ctr) - sorted_node_end_dict(token_node.token.tokenID.toInt) - cross_store
                cross_ctr += 1
              }
              //commendted out for adding argrel
              //            s"${
              //              "　" * token_node.space + token_node.surface_string + "ー" * {
              //                sorted_node_end_dict(token_node.token_id) - token_node.space - token_node.surface_string.length
              //              } + "｜" + cross_str.toString}"
              //            println("debug"+token_node.token.entityID)
              //            println("debug"+token_node.argRels.map{argRel => "（"+"%s".format(argRel.role)+"%s".format(argRel.entityID)+"）"}.mkString("、"))
              s"${
                "　" * node_space_dict(token_node.token.tokenID.toInt) + transliterator.transliterate(token_node.token.entityID) + "）" + token_node.token.surfaceString + transliterator.transliterate(token_node.argRels.map { argRel => "（" + "%s".format(argRel.role) + "%s".format(argRel.entityID) + "）" }.mkString) + "ー" * {
                  sorted_node_end_dict(token_node.token.tokenID.toInt) - node_space_dict(token_node.token.tokenID.toInt) - token_node.token.entityID.length - 1 - token_node.token.surfaceString.length - token_node.argRels.map { argRel => "（" + "%s".format(argRel.role) + "%s".format(argRel.entityID) + "）" }.mkString.length
                } + "｜" + cross_str.toString
              }"
            } else {
              //commented out for adding argrel
              //            s"${
              //              "　" * token_node.space + token_node.surface_string + "ー" * {
              //                sorted_node_end_dict(token_node.token_id) - token_node.space - token_node.surface_string.length
              //              } + "｜"
              //            }"
              s"${
                "　" * node_space_dict(token_node.token.tokenID.toInt) + transliterator.transliterate(token_node.token.entityID) + "）" + token_node.token.surfaceString + transliterator.transliterate(token_node.argRels.map { argRel => "（" + "%s".format(argRel.role) + "%s".format(argRel.entityID) + "）" }.mkString) + "ー" * {
                  sorted_node_end_dict(token_node.token.tokenID.toInt) - 1 - token_node.token.entityID.length - node_space_dict(token_node.token.tokenID.toInt) - token_node.token.surfaceString.length - token_node.argRels.map { argRel => "（" + "%s".format(argRel.role) + "%s".format(argRel.entityID) + "）" }.mkString.length
                } + "｜"
              }"
            }
          } //commented out for addring argrels
          //          else {s"${"　"*token_node.space + token_node.surface_string}\n"}
          else { s"${"　" * node_space_dict(token_node.token.tokenID.toInt) + transliterator.transliterate(token_node.token.entityID) + "）" + token_node.token.surfaceString + transliterator.transliterate(token_node.argRels.map { argRel => "（" + "%s".format(argRel.role) + "%s".format(argRel.entityID) + "）" }.mkString)}\n" }
      }
    val token_tree_feat =
      tokens map {
        token_node =>
          if (sorted_node_end_dict_feat contains token_node.token.tokenID.toInt) {
            if (node_cross_dict_feat contains token_node.token.tokenID.toInt) {
              val cross_str = new StringBuilder
              var cross_ctr: Int = 0
              var cross_store: Int = 0
              for (cross <- node_cross_dict_feat { token_node.token.tokenID.toInt }) yield {
                cross_str ++= "　" * { node_cross_dict_feat(token_node.token.tokenID.toInt)(cross_ctr) - sorted_node_end_dict_feat(token_node.token.tokenID.toInt) - cross_store - 1 } + "｜"
                cross_store += node_cross_dict_feat(token_node.token.tokenID.toInt)(cross_ctr) - sorted_node_end_dict_feat(token_node.token.tokenID.toInt) - cross_store
                cross_ctr += 1
              }

              s"${
                "　" * node_space_dict_feat(token_node.token.tokenID.toInt) + token_node.token.lemma + transliterator.transliterate({ Array(token_node.token.pos, token_node.token.coarsePos, token_node.token.voice, token_node.token.ne, token_node.token.negated) ++: token_node.token.features.toArray }.map { feat => "（" + "%s".format(feat) + "）" }.mkString) + "ー" * {
                  sorted_node_end_dict_feat(token_node.token.tokenID.toInt) - node_space_dict_feat(token_node.token.tokenID.toInt) - token_node.token.lemma.length - { Array(token_node.token.pos, token_node.token.coarsePos, token_node.token.voice, token_node.token.ne, token_node.token.negated) ++: token_node.token.features.toArray }.map { feat => "（" + "%s".format(feat) + "）" }.mkString.length
                } + "｜" + cross_str.toString
              }"
            } else {

              s"${
                "　" * node_space_dict_feat(token_node.token.tokenID.toInt) + token_node.token.lemma + transliterator.transliterate({ Array(token_node.token.pos, token_node.token.coarsePos, token_node.token.voice, token_node.token.ne, token_node.token.negated) ++: token_node.token.features.toArray }.map { feat => "（" + "%s".format(feat) + "）" }.mkString) + "ー" * {
                  sorted_node_end_dict_feat(token_node.token.tokenID.toInt) - node_space_dict_feat(token_node.token.tokenID.toInt) - token_node.token.lemma.length - { Array(token_node.token.pos, token_node.token.coarsePos, token_node.token.voice, token_node.token.ne, token_node.token.negated) ++: token_node.token.features.toArray }.map { feat => "（" + "%s".format(feat) + "）" }.mkString.length
                } + "｜"
              }"
            }
          } else { s"${"　" * node_space_dict_feat(token_node.token.tokenID.toInt) + token_node.token.lemma + transliterator.transliterate({ Array(token_node.token.pos, token_node.token.coarsePos, token_node.token.voice, token_node.token.ne, token_node.token.negated) ++: token_node.token.features.toArray }.map { feat => "（" + "%s".format(feat) + "）" }.mkString)}\n" }
      }
    "====dep tree w sem====\n" + token_tree.mkString("\n") + "====dep tree w feat====\n" + token_tree_feat.mkString("\n")
  }
  override def toString: String = {

    //      println ("debug"+tokens.last.local_node_end_dict)

    val token_str =
      tokens map {
        token_node =>
          val argrel_str = token_node.argRels.map("%s".format(_))
          s"${token_node.token}\n  " + (token_node.depRel.toString +: argrel_str).mkString("\n")
      }
    token_str.mkString("\n")
  }
}

// Interface to run KNP
class KNP(val jumanCommand: String, val knpCommand: String) {
  // "-i #" is to regard line starting with "#" as a comment
  //private[this] lazy val juman_process = new ProcessBuilder(jumanCommand.split("""\s+""") ++ Seq("-i", "#"): _*).start()
  private[this] lazy val juman_process = new ProcessBuilder(jumanCommand.split("""\s+"""): _*).start()
  //private[this] lazy val juman_input_stream = Source.fromInputStream(juman_process.getInputStream, "UTF-8")
  private[this] lazy val juman_input_stream = new BufferedReader(new InputStreamReader(juman_process.getInputStream, "UTF-8"))
  private[this] lazy val juman_output_stream = new BufferedWriter(new OutputStreamWriter(juman_process.getOutputStream, "UTF-8"))

  private[this] lazy val knp_process = new ProcessBuilder(knpCommand.split("""\s+""") ++ Seq("-tab", "-anaphora"): _*).start()
  //private[this] lazy val knp_input_stream = Source.fromInputStream(knp_process.getInputStream, "UTF-8")
  private[this] lazy val knp_input_stream = new BufferedReader(new InputStreamReader(knp_process.getInputStream, "UTF-8"))
  private[this] lazy val knp_output_stream = new BufferedWriter(new OutputStreamWriter(knp_process.getOutputStream, "UTF-8"))

  /**
   * juman/KNP で解析しやすいように、入力テキストを前処理する（半角を全角に、etc.）
   * @param text 入力テキスト
   * @return 正規化したテキスト
   */
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

  /**
   * グローバルに unique な document ID を生成する
   * TODO: 並列処理するために排他処理が必要
   * @return
   */
  def getNewId: Int = {
    val id = current_id
    current_id += 1
    id
  }
  private var current_id = 0

  /**
   * プロセス間通信で juman にテキストを渡し、解析結果を受け取る
   * TODO: 並列処理のためには排他処理が必要
   * @param sentence
   * @return
   */
  private def runJuman(sentence: String): Array[String] = {
    juman_output_stream.write(sentence)
    juman_output_stream.newLine()
    juman_output_stream.flush()
    val juman_lines: Array[String] =
      //(for (line <- juman_input_stream.getLines().takeWhile(_ != "EOS")) yield {
      (for (line <- Iterator.continually(juman_input_stream.readLine()).takeWhile(_ != "EOS")) yield {
        //println(line)
        line
      }).toArray
    juman_lines
  }

  /**
   * プロセス間通信で KNP に文（juman の解析結果）を渡し、解析結果を受け取る
   * ドキュメントレベルの照応解析をするため、同じドキュメント間では同じ doc_id を渡す
   * @param docID
   * @param sentenceID
   * @param jumanSentence
   * @return
   */
  private def runKNPSentence(docID: Int, sentenceID: Int, jumanSentence: Array[String]): Array[String] = {
    // write KNP stream the document ID and output from Juman
    knp_output_stream.write(f"# S-ID:$docID%d-$sentenceID%d JUMAN-PIPELINE")
    knp_output_stream.newLine()
    for (juman_line <- jumanSentence) {
      //println(juman_line)
      knp_output_stream.write(juman_line)
      knp_output_stream.newLine
    }
    knp_output_stream.write("EOS")
    knp_output_stream.newLine
    knp_output_stream.flush()
    // read outputs from KNP
    val knp_output: Array[String] =
      //(for (line <- knp_input_stream.getLines().takeWhile(_ != "EOS")) yield {
      (for (line <- Iterator.continually(knp_input_stream.readLine()).takeWhile(_ != "EOS")) yield {
        //println(line)
        line
      }).toArray
    knp_output
  }

  /**
   * ドキュメント（文の集合）を KNP で解析する
   * ドキュメント内で共通の doc_id を指定することで、KNP にドキュメント内照応解析をさせる
   * See: http://www.lr.pi.titech.ac.jp/~sasano/knp/input.html
   * @param jumanOutputs
   * @return
   */
  private def runKNPDocument(jumanOutputs: Array[Array[String]]): Array[Array[String]] = {
    // get a new document ID
    val doc_id = getNewId

    // run KNP for each sentence
    val knp_outputs =
      for ((juman_sentence, sentence_id) <- jumanOutputs.zipWithIndex) yield {
        runKNPSentence(doc_id, sentence_id, juman_sentence)
      }
    knp_outputs
  }

  /**
   * KNP の出力から基本句の行、それを構成する自立語と全ての語の行を Array にして返す
   * @param knpOutput KNPからの出力（行の配列）
   * @return (基本句の行、自立語の行の配列、全ての語の行の配列)
   */
  // 基本句と、それに所属する自立語、全ての語の行を triple にして返す
  private def getKnpJumanTriple(knpOutput: Array[String]): Array[(String, Array[String], Array[String])] = {
    val kihonku_token_lines = knpOutput.filterNot("#* " contains _(0)) // 文情報・文節情報の行を除く TODO: なぜかスペースで始まる行があるので、とりあえず除いておく。原因を後で調べる。
    var i = 0
    val pairs = ArrayBuffer.empty[(String, Array[String], Array[String])]
    while (i < kihonku_token_lines.size) {
      //println(kihonku_token_lines(i))
      assert(kihonku_token_lines(i)(0) == '+')
      val kihonku_line = kihonku_token_lines(i)
      i += 1
      val juman_line_begin = i
      while (i < kihonku_token_lines.size &&
        kihonku_token_lines(i)(0) != '+') i += 1 // 次の基本句行まで読み飛ばす
      val juman_line_end = i
      // juman_line_begin から juman_line_end までで、自立語の行を取ってくる
      val juman_lines = kihonku_token_lines.slice(juman_line_begin, juman_line_end)
      val jiritsugo_lines = juman_lines.filterNot(List("助詞", "特殊") contains _.split(' ')(3))
      if (jiritsugo_lines.isEmpty) {
        // 自立語がない -> 全部の行を返す
        pairs += ((kihonku_line, juman_lines, juman_lines))
      } else {
        // 自立語がある -> 自立語の行だけ返す
        pairs += ((kihonku_line, jiritsugo_lines, juman_lines))
      }
    }
    pairs.toArray
    /*
    // 以下のコードは、基本句が付属語しか含んでいないときに exception を起こす
    var i = 0
    val pairs = ArrayBuffer.empty[(String, Array[String])]
    while (i < knp_output.size) {
      if (knp_output(i)(0) != '+') {
        // 基本句まで読み飛ばす
        i += 1
      } else {
        val knp_line = knp_output(i)
        i += 1
        while (List("助詞", "特殊") contains knp_output(i).split(' ')(3)) i += 1 // 付属語を読み飛ばす
        val kihonku_begin = i
        while (!("*+" contains knp_output(i)(0)) && // 次の基本句・文節まで読む
          !(List("助詞", "特殊") contains knp_output(i).split(' ')(3)) && // 付属語まで読む
          !(List("動詞性接尾辞", "形容詞性述語接尾辞") contains knp_output(i).split(' ')(5))) // 「れる」なども付属語（名詞正接尾辞は入れる）
          i += 1
        val juman_lines = knp_output.slice(kihonku_begin, i)
        assert(juman_lines.nonEmpty)
        pairs += ((knp_line, juman_lines))
      }
    }
    pairs.toArray
    */
  }

  /**
   * KNP の出力（各行の配列）から、構文木・述語項構造のデータを作る
   * @param knpOutput KNPの出力
   * @return 構文木・述語項構造のデータ構造（KNP解析木）
   */
  private def knp2Sentence(knpOutput: Array[String]): KNP解析木 = {
    // KNP と Juman の解析結果をペアにする
    val knp_juman_triples = getKnpJumanTriple(knpOutput)
    val token_nodes: Array[基本句ノード] =
      for (((knp_line, jiritsugo_lines, juman_lines), token_id) <- knp_juman_triples.zipWithIndex) yield {
        // *** Juman の出力を解析
        val juman_cols = jiritsugo_lines map (_.split(' '))
        val lemma = juman_cols.map(_(2)).mkString("")
        val last_morph_cols = juman_cols.last
        // 主辞の品詞を取る
        val pos = if (last_morph_cols(5) == "*") last_morph_cols(3) else last_morph_cols(5)
        // 付属語が・を含むときは並列句だと思う
        val is_heuristic_coordination = juman_lines.exists(_.split(' ')(0) == "・")
        // *** KNP の出力を解析
        val knp_cols = knp_line.split(" ")
        require(knp_cols.length == 3)
        //val dependency = cols(1)
        val features = "<(.*?)>".r.findAllMatchIn(knp_cols(2)).map(_.group(1))
        val feature_map =
          (features map (_.split(':').toArray) collect {
            // TODO: による、である、etc.
            case Array("正規化代表表記", lemma)    => "lemma" -> lemma.split('+').map(_.split('/')(0)).mkString("")
            case Array("体言")                => "coarse_pos" -> "体言"
            case Array("用言", _)             => "coarse_pos" -> "用言"
            case Array("態", voice)          => "voice" -> voice
            case Array("NE内", ne)           => "marker" -> "ne_internal"
            case Array("NE", ne, _)         => "ne" -> ne
            case Array("EID", entity_id)    => "entity_id" -> entity_id
            case Array("解析格", kaku)         => "marker" -> kaku
            //            case Array("ガ")                 => "marker" -> "ga"
            //            case Array("ヲ")                 => "marker" -> "o"
            //            case Array("ニ")                 => "marker" -> "ni"
            //            case Array("デ")                 => "marker" -> "de"
            //            case Array("ト")                 => "marker" -> "to"
            case Array("ハ")                 => "topic" -> "topic"
            case Array("係", "同格連体")         => "marker" -> "appos"
            case Array("係", "ノ格")           => "marker" -> "ノ"
            case Array("係", "文節内")          => "marker" -> "internal"
            case Array("否定表現")              => "neg" -> "neg"
            case Array("述語項構造", _, _, args) => "args" -> args
            case Array("複合辞")               => "MWE" -> "MWE"
            case Array("ID", "〜て以来")        => "after" -> "after"
          }).toMap
        //        println(feature_map)
        // Create a token
        val token_id_str = token_id.toString
        val coarse_pos = feature_map.getOrElse("coarse_pos", "")
        val voice = feature_map.getOrElse("voice", "")
        val ne = feature_map.getOrElse("ne", "")
        val negated = feature_map contains "neg"
        val other_features = feature_map.keys filter { Set("MWE", "after") contains _ }
        val surface_string = juman_lines.map(_.split(' ')).map(_(0)).mkString("")
        val entity_id = feature_map("entity_id")
        val token = 基本句(token_id_str, feature_map.getOrElse("lemma", lemma), pos, coarse_pos, voice, ne, negated, other_features.toArray, surface_string, entity_id)
        // Create a dependency relation
        val (head_id, dep_type) = knp_cols(1).span("-0123456789" contains _)
        val dep_marker = if (is_heuristic_coordination || dep_type == "P")
          "coord" // 並列
        else
          feature_map.getOrElse("marker", // 格表示があればそれを使う
            feature_map.getOrElse("topic", "other"))
        val deprel = 係り受け(dep_marker, head_id)
        //        for debugging only
        //        val local_head_id = head_id.toInt
        // Create a argument relation
        val argrels =
          if (feature_map contains "args") {
            feature_map("args").split(';') map {
              rel_str =>
                val cols = rel_str.split('/')
                require(cols.length == 4)
                述語項関係(cols(0), cols(3))
            }
          } else
            Array.empty[述語項関係]
        基本句ノード(token, deprel, argrels)
      }
    //println(KNP解析木(token_nodes))
    KNP解析木(token_nodes)
  }

  /**
   * テキストに対して、文区切り、juman, KNP をかけ、結果の構文木・述語項構造を返す
   * 一文に対して一つ KNP解析木 が返される
   * @param text 入力テキスト
   * @return 解析結果
   */
  def parse(text: String): Array[KNP解析木] = {
    // sentence spliting
    // currently, newline and "。" is regarded as sentence boundaries
    val sentences: Array[String] = normalize(text).split("""\n+|。\n*""").map(_ + "。")
    //sentences foreach println

    // run Juman and get result as Array[String]
    val juman_outputs = sentences map runJuman

    // put Juman output into KNP and get output from KNP
    val knp_outputs = runKNPDocument(juman_outputs)

    // convert KNP output into Sentence
    //knp_lines foreach println
    // extract base token information
    val parsed_sentences: Array[KNP解析木] =
      knp_outputs map knp2Sentence

    //    parsed_sentences map (prettyPrint(_))
    parsed_sentences

  }
}

/**
 * KNPクラスのテスト
 * デフォルトの juman, knp を使って、コマンドライン引数で与えられたテキストを解析して結果を表示する
 */
object KNPTest {
  def main(args: Array[String]): Unit = {
    val knp = new KNP("juman", "knp")
    for (arg <- args) {
      val parses = knp.parse(arg)
      parses foreach println
    }
  }
}
/**
 * object prettyPrint {
 * def apply(sentence: KNP解析木): KNP解析木 = {
 * var head_deps_dict = collection.mutable.Map[Int, collection.mutable.Buffer[Int]]()
 * var head_list = collection.mutable.Buffer[Int]()
 * var node_loc_dict = collection.mutable.Map[Int, Int]()
 * var node_loc_dict_feat = collection.mutable.Map[Int, Int]()
 * var space_accumulator = 0
 * var space_accumulator_feat = 0
 * sentence.tokens.foreach((token) => {
 * token.depRel match {
 * case 係り受け(_, head_id) => {
 * head_list += head_id.toInt
 * if (head_deps_dict contains head_id.toInt) {
 * head_deps_dict(head_id.toInt) += token.token.tokenID.toInt
 * }
 * else {
 * head_deps_dict += (head_id.toInt -> collection.mutable.Buffer(token.token.tokenID.toInt))
 * }
 * }
 * }
 *
 * if (head_list contains token.token.tokenID.toInt) {
 * space_accumulator_feat = {head_deps_dict(token.token.tokenID.toInt) map (node_loc_dict_feat(_))}.max
 * space_accumulator = {
 * head_deps_dict(token.token.tokenID.toInt) map (node_loc_dict(_))
 * }.max
 * for (token <- head_deps_dict(token.token.tokenID.toInt)) yield {
 * sentence.node_end_dict(token) = space_accumulator
 * sentence.node_end_dict_feat(token) = space_accumulator_feat
 * }
 * }
 * else {
 * space_accumulator = 0
 * space_accumulator_feat = 0
 * }
 * sentence.node_space_dict(token.token.tokenID.toInt) = space_accumulator
 * sentence.node_space_dict_feat(token.token.tokenID.toInt) = space_accumulator_feat
 * node_loc_dict_feat += (token.token.tokenID.toInt->{space_accumulator_feat+token.token.lemma.length()+ {Array(token.token.pos,token.token.coarsePos, token.token.voice, token.token.ne, token.token.negated)++:token.token.features.toArray}.map{feat => "（"+"%s".format(feat)+"）"}.mkString.length})
 * node_loc_dict += (token.token.tokenID.toInt -> {
 * space_accumulator + 1+ token.token.entityID.length()+token.token.surfaceString.length()+token.argRels.map{argRel => "（"+"%s".format(argRel.role)+"%s".format(argRel.entityID)+"）"}.mkString.length
 * })
 * })
 * sentence.toString
 * sentence
 * }
 * }
 */

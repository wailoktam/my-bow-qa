package qa.main.ja

import java.io.{ FileWriter, BufferedWriter, File }
import org.atilika.kuromoji.Tokenizer
import org.atilika.kuromoji.Token
import com.ibm.icu.text.Normalizer2

object SharedFunctions {

  def myNormalize(input: String): String = {
    val tokenizer = Tokenizer.builder.mode(Tokenizer.Mode.NORMAL).build
    val tokens = tokenizer.tokenize(input).toArray
    val normalizer = Normalizer2.getNFCInstance()
    (tokens.map { t => normalizer.normalize(t.asInstanceOf[Token].getSurfaceForm()) }).mkString
  }

  def powerTrim(text: String): String = {
    val fullSpaceRe = """　""".r
    fullSpaceRe.replaceAllIn(text.trim(), "")
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def makeFileList(dirName: String): Array[String] = {
    recursiveListFiles(new File(dirName)).filter(_.isFile).map(_.getAbsolutePath)
  }
}



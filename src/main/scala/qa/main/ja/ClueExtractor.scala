package qa.main.ja

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import com.ibm.icu.text.Transliterator
import scala.util.matching.Regex
import sys.process._
import java.io.File
import scala.xml.factory.XMLLoader
import scala.xml._
import scala.io.Source

object XMLLoaderIgnoringDTD extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.setValidating(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}

class ClueExtractor(val)
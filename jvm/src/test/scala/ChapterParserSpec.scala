package edu.holycross.shot.ptolemy

import org.scalatest.FlatSpec
import scala.xml._

class ChapterParserSpec extends FlatSpec {

  val f = "tei/tlg0363.tlg009.epist03-p5-u8.xml"
  val root = XML.loadFile(f)

  "The TeiParser object" should "parse a <div> element representing a chapter" in {
    val books = root \ "text"  \ "body" \ "div"
    val bk2 = books(1)
    val chaps = bk2 \ "div"
    val parsed = TeiParser.parseChapter(chaps(1), "Europe", "2")
    println(parsed.size + " items in 2.2")
  }
}

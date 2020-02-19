package edu.holycross.shot.ptolemy

import org.scalatest.FlatSpec
import scala.xml._


class TeiParserSpec extends FlatSpec {
  val f = "tei/tlg0363.tlg009.epist03-p5-u8.xml"

  "The TeiParser object" should "parse a whole TEI document" in {

    val root = XML.loadFile(f)
    val parsed = TeiParser.parseTEI(root)

  }
}

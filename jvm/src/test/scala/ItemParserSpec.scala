package edu.holycross.shot.ptolemy

import org.scalatest.FlatSpec
import scala.xml._

class ItemParserSpec extends FlatSpec {

  val noFractionsXml = """<item>
      <name  key='pt_ll_1' type='place'>Βόρειον ἄκρον</name>
      <measure  type='llpair'>
          <num  type='cardinal'>ια</num>
          <num  type='fraction'/>
          <num  type='cardinal'>ξα</num>
          <num  type='fraction'/>
      </measure>
  </item>
"""
  val withFractionsXml = """<item >
      <name  key='pt_ll_8' type='place'>Μάγνατα πόλις [ἐπίσημος]</name>
      <measure  type='llpair'>
          <num  type='cardinal'>ια</num>
          <num  type='fraction'>δ</num>
          <num  type='cardinal'>ξ</num>
          <num  type='fraction'>δ</num>
      </measure>
  </item>
"""
  val withFunnyCharsXml = """<item >
            <name  key='pt_ll_2' type='place'>Οὐεννίκνιον ἄκρον</name>

            <measure  type='llpair'>
                <num  type='cardinal'>ιβ</num>
                <num  type='fraction'>𐅵 γ</num>
                <num  type='cardinal'>ξα</num>
                <num  type='fraction'>γ</num>
            </measure>
        </item>
"""


  "The TeiParser object" should "parse an <item> element with empty fractional components" in {
    val item = XML.loadString(noFractionsXml)
    val delimited = TeiParser.parseItem(item)
    val expectedColumns = 6
    assert(delimited.split("#").size == expectedColumns)
  }

  it should "parse an <item> element with full fractional components" in {
    val item = XML.loadString(withFractionsXml)
    val delimited = TeiParser.parseItem(item)
    val expectedColumns = 6
    assert(delimited.split("#").size == expectedColumns)
  }

  it should "parse an <item> element including unicode code points MBP" in {
    val item = XML.loadString(withFunnyCharsXml)
    val delimited = TeiParser.parseItem(item)
    val expectedColumns = 6
    assert(delimited.split("#").size == expectedColumns)
  }


}

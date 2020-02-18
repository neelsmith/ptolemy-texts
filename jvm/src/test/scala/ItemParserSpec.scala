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


  "The TeiParser object" should "parse an <item> element with empty fractional components" in {
    val item = XML.loadString(noFractionsXml)
    val delimited = TeiParser.parseItem(item)
    val expectedColumns = 8
    println(delimited.split("#").size + " cols")

    //pt_ll_1#Βόρειον ἄκρον#ιαʹ#ξαʹ#11.0#61.0
  }


}

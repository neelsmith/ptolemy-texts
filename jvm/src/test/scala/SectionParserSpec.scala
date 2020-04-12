package edu.holycross.shot.ptolemy

import org.scalatest.FlatSpec
import scala.xml._

class SectionParserSpec extends FlatSpec {

  val sectXml = """<div  n='1' type='paralios'>
      <p > Ἀρκτικῆς πλευρᾶς περιγραφὴ, ἦς ὑπέρκειται <placeName >Ὠκεανὸς
              Ὑπερβόρειος</placeName>.</p>
      <list  type='simple'>
          <item  n='submitted'>
              <name  key='pt_ll_1' type='place'>Βόρειον ἄκρον</name>
              <measure  type='llpair'>
                  <num  type='cardinal'>ια</num>
                  <num  type='fraction'/>
                  <num  type='cardinal'>ξα</num>
                  <num  type='fraction'/>
              </measure>
          </item>
          <item >
              <name  key='pt_ll_2' type='place'>Οὐεννίκνιον ἄκρον</name>
              <measure  type='llpair'>
                  <num  type='cardinal'>ιβ</num>
                  <num  type='fraction'>𐅵 γ</num>
                  <num  type='cardinal'>ξα</num>
                  <num  type='fraction'>γ</num>
              </measure>
          </item>
          <item >
              <name  key='pt_ll_3' type='place'>Οὐιδούα ποταμοῦ ἐκβολαί</name>
              <measure  type='llpair'>
                  <num  type='cardinal'>ιγ</num>
                  <num  type='fraction'/>
                  <num  type='cardinal'>ξα</num>
                  <num  type='fraction'/>
              </measure>
          </item>
          <item >
              <name  key='pt_ll_4' type='place'>Ἀργίτα ποταμοῦ ἐκβολαί</name>
              <measure  type='llpair'>
                  <num  type='cardinal'>ιδ</num>
                  <num  type='fraction'>𐅵 </num>
                  <num  type='cardinal'>ξα</num>
                  <num  type='fraction'>𐅵 </num>
              </measure>
          </item>
          <item >
              <name  key='pt_ll_5' type='place'>Ῥοβόγδιον ἄκρον</name>
              <measure  type='llpair'>
                  <num  type='cardinal'>ιϚ</num>
                  <num  type='fraction'>γ</num>
                  <num  type='cardinal'>ξα</num>
                  <num  type='fraction'>𐅵 </num>
              </measure>
          </item>
      </list>
  </div>
"""
  val sect = XML.loadString(sectXml)

  "The TeiParser object" should "parse a <div> element representing a geo type" in {
    val parsed = TeiParser.parseSection(sect, "Europe", "hibernia", "2.2")
    println(parsed)
  }
}

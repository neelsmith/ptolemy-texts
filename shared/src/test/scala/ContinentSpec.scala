package edu.holycross.shot.ptolemy

import org.scalatest.FlatSpec

class ContinentSpec extends FlatSpec {

  "The Continent trait" should "include Ptolemy's three continents" in {
    val europe: Continent = Europe
    val asia: Continent = Asia
    val africa: Continent = Africa
    europe match {
      case c: Continent => {
        assert(true)
      }
      case _ => fail(s"${europe} not recognized as a Continent!")
    }
    asia match {
      case c: Continent => {
        assert(true)
      }
      case _ => fail(s"${europe} not recognized as a Continent!")
    }
    africa match {
      case c: Continent => {
        assert(true)
      }
      case _ => fail(s"${europe} not recognized as a Continent!")
    }
  }

  it should "require a name" in {
    assert (Europe.name == "Europe")
    assert (Asia.name == "Asia")
    assert (Africa.name == "Africa")
  }



}

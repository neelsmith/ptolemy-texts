package edu.holycross.shot.ptolemy

import edu.holycross.shot.greek._

import wvlet.log._
import wvlet.log.LogFormatter.SourceCodeLogFormatter

case class PtolemyString (
  passage: String,
  continent: String,
  province: String,
  siteType: String,
  id: String,
  text: String,
  lonStr: String,
  latStr: String,
  lon: Double,
  lat: Double) {

  }

object PtolemyString extends LogSupport {

  def apply(line : String) : PtolemyString = {
    val cols = line.trim.split("#")
    if (cols.size < 10) {
      error("Two few columns in " + line)
      PtolemyString("","","","","","","","",-1.0, -1.0)
    } else {
    //println("Parsing " + cols.toVector)
    val lon = {
      try {
        cols(8).toDouble
      } catch {
        case t: Throwable => -1.0
      }
    }
    val lat = {
      try {
        cols(9).toDouble
      } catch {
        case t: Throwable => -1.0
      }
    }
    PtolemyString(
      cols(0),
      cols(1),
      cols(2),
      cols(3),
      cols(4),
      cols(5),
      cols(6),
      cols(7),
      lon,
      lat
    )
  }
}
}

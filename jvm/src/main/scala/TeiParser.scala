package edu.holycross.shot.ptolemy

import edu.holycross.shot.greek._

import wvlet.log._
import wvlet.log.LogFormatter.SourceCodeLogFormatter



/** Factory object for generating objects from TEI
* XML source.
*/
object TeiParser extends LogSupport {

  Logger.setDefaultLogLevel(LogLevel.DEBUG)

  /** Parse a Vector of four TEI <num> elements,
  * and format the resulting lon-lat data as
  * a delimited-text String.
  *
  * @param nums A Vector containing 4 TEI <num> elements.
  */
  def parseLonLat(nums: Vector[scala.xml.Node]): String = {
    debug("Parse nums "  + nums)
    val lonDeg = if (nums(0).text.trim.isEmpty) {""} else {
      nums(0).text + "' "
    }
    val lonMin = if (nums(1).text.trim.isEmpty) {""} else {
      nums(1).text + "\""
    }
    val latDeg = if (nums(2).text.trim.isEmpty) {""} else {
      nums(2).text + "' "
    }
    val latMin = if (nums(3).text.trim.isEmpty) {""} else {
      nums(3).text + "\""
    }
    val lon = MilesianNumeric(lonDeg + lonMin)
    val lat = MilesianNumeric(latDeg + latMin)
    val lonStr = lon.ucode
    val latStr = lat.ucode
    debug("Created lon/lat pair " + lon + " : " + lat)
    //val lonStr = "X" // if (lon.ucode.isEmpty) { " X "} else { lon.ucode }
    //val latStr = "X"  //if (lat.ucode.isEmpty) { " X "} else { lat.ucode }
    //debug("dispaly strings " + lonStr + " : " + latStr)
    val delimited = Vector(lonStr,latStr,lon.toDouble,lat.toDouble).mkString("#")
    debug("DELIMITED: " + delimited)
    delimited
  }


  /** Parse a TEI <item> element, and format
  * a delimited-text String.
  *
  * @param n A TEI <item> element.
  */
  def parseItem(n: scala.xml.Node) : String = {
    debug("PARSE n = " + n)
    val nameSeq = n \ "name"
    if(nameSeq.toVector.nonEmpty) {
      val nd = nameSeq.head

      val processed = nd.attribute("type").get.text match {
        case "episemos" => {
          debug("episemos: " + nd)
          ""
        }
        case "place" => {

          val measures = (n \ "measure" \ "num").toVector

          val id = nd.attribute("key").get.text
          debug("Parse a place with id " + id + " from measures " + measures)
          val res = id + "#" + nd.text.replaceAll("[\\s]+", " ")
          val lldata = if (measures.size != 4) {
            error(s"ERROR: ${measures.size} nums in  " + res)
            ""
          } else {
            val ll = parseLonLat(measures)
            debug("Parsed ll as " + ll)
            ll
          }
          res + "#" + lldata
        }
      }
      processed
    } else {
      error("No name element.")
      ""
    }
  }


  /** Groups together a Vector al list elements for a given section to go
  * along with higher-level data applying to all items in the lists.
  *
  * @param sect A parsed TEI <div> representing a citable section, the third
  * tier in the citation hierarchy for the Geography.
  * @param continent Value for continent  containing this section.
  * @param province Value for province containing this section.
  * @param bkChap Book.chapter value for province containing this section.
  */
  def parseSection(sect: scala.xml.Node,
    continent: String,
    province: String,
    bkChap: String ) :  (String, Vector[scala.xml.Node]) = {
    val sectNum = sect.attribute("n").get
    val geoType = sect.attribute("type").getOrElse("")

    //val props = if (province.isEmpty) { "" } else {Vector(continent, province, geoType).mkString("#") }
    val summary = Vector(bkChap + "." + sectNum, continent, province, geoType).mkString("#")
    val lists =  sect \ "list"
    //val listData =   .flatten.flatten.toVector.filter(_._2.nonEmpty)
    (summary, lists.toVector)
  }

}

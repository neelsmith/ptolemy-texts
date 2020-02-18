package edu.holycross.shot.ptolemy

import edu.holycross.shot.greek._


/** Factory object for generating objects from TEI
* XML source.
*/
object TeiParser {


  /** Parse a Vector of four TEI <num> elements,
  * and format the resulting lon-lat data as
  * a delimited-text String.
  *
  * @param nums A Vector containing 4 TEI <num> elements.
  */
  def parseLonLat(nums: Vector[scala.xml.Node]): String = {
    val lonDeg = if (nums(0).text.isEmpty) {""} else {
      nums(0).text + "' "
    }
    val lonMin = if (nums(1).text.isEmpty) {""} else {
      nums(1).text + "\""
    }
    val latDeg = if (nums(2).text.isEmpty) {""} else {
      nums(2).text + "' "
    }
    val latMin = if (nums(3).isEmpty) {""} else {
      nums(3).text + "\""
    }
    val lon = MilesianNumeric(lonDeg + lonMin)
    val lat = MilesianNumeric(latDeg + latMin)
    //s"${lon.ucode} =  ${lon.toDouble} : ${lat.ucode} = ${lat.toDouble}"
    Vector(lon.ucode,lat.ucode,lon.toDouble,lat.toDouble).mkString("#")
  }

/*
  def itemProcess(n: scala.xml.Node) : String = {
    val nameSeq = n \ "name"
    if(nameSeq.toVector.nonEmpty) {
      val nd = nameSeq.head
      val processed = nd.attribute("type").get.text match {
        case "episemos" => "" //println("Episemos")
        case "place" => {
          val measures = (n \ "measure" \ "num").toVector
          val id = nd.attribute("key").get.text
          val res = id + "#" + nd.text.replaceAll("[\\s]+", " ")
          val lldata = if (measures.size != 4) {
            println(s"ERROR: ${measures.size} nums in  " + res)
            ""
          } else {
            parseLonLat(measures)
          }
          res + "#" + lldata
        }
      }
      processed
    } else {

      ""
    }
  }
*/

  /** Parse a TEI <item> element, and format
  * a delimited-text String.
  *
  * @param n A TEI <item> element.
  */
  def parseItem(n: scala.xml.Node) : String = {
    val nameSeq = n \ "name"
    if(nameSeq.toVector.nonEmpty) {
      val nd = nameSeq.head
      val processed = nd.attribute("type").get.text match {
        case "episemos" => "" //println("Episemos")
        case "place" => {
          val measures = (n \ "measure" \ "num").toVector
          val id = nd.attribute("key").get.text
          val res = id + "#" + nd.text.replaceAll("[\\s]+", " ")
          val lldata = if (measures.size != 4) {
            println(s"ERROR: ${measures.size} nums in  " + res)
            ""
          } else {
            parseLonLat(measures)
          }
          res + "#" + lldata
        }
      }
      processed
    } else {
      ""
    }
  }
}

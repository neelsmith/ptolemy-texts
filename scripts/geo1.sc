import scala.xml._
import edu.holycross.shot.greek._

val xml = "tei/tlg0363.tlg009.epist03-p5-u8.xml"


val root = XML.loadFile(xml)
val books = root \ "text" \ "body" \ "div"

def lists(bkList : scala.xml.NodeSeq) = {
 for (bk <- bkList) yield {
  val bkNum = bk.attribute("n").get
  val continentOpt =   bk.attribute("ana")
  val continent = continentOpt match {
    case None => ""
    case _ => continentOpt.get.text.replaceAll("#", "")
  }
  val chaps = bk \ "div"
  for (ch <- chaps) yield {
    val chNum = ch.attribute("n").get

    val provinceOpt =   ch.attribute("ana")
    val province = provinceOpt match {
      case None => ""
      case _ => provinceOpt.get.text.replaceAll("#", "")
    }

    val sects = ch \ "div"
    for (sect <- sects) yield {
      val sectNum = sect.attribute("n").get
      val geoType = sect.attribute("type").getOrElse("")

      val props = if (province.isEmpty) { "" } else {Vector(continent, province, geoType).mkString("#") }
      val summary = Vector(bkNum, chNum, sectNum).mkString(".") + "#" + props
      val lists =  sect \ "list"
      (summary, lists.toVector)
    }
  }
} }



val listData = lists(books).flatten.flatten.toVector.filter(_._2.nonEmpty)


case class PtolStrings (
  psg: String,
  continent: String,
  province: String,
  siteType: String,
  id: String,
  text: String,
  lonStr: String,
  latStr: String,
  lon: Double,
  lat: Double
)

def parseCsv(s: String): PtolStrings = {
  val cols = s.split("#")
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
  PtolStrings(
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

def formatLL(nums: Vector[scala.xml.Node]): String = {
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
          formatLL(measures)
        }
        res + "#" + lldata
      }
    }
    processed
  } else {

    ""
  }
}

def listProcess(l : scala.xml.Node) = {
  val itemNodes = l \ "item"
  val items = itemNodes.toVector

  val processed = for (i <- items) yield {
    val myItem = itemProcess(i)
    myItem
  }
  processed
}


def csv(data: Vector[(String, Vector[scala.xml.Node])]) = {
  val ptData = for (lData <- data) yield {
    //println(lData._1 + " " + lData._2.size)
    for (l <- lData._2) yield {
      listProcess(l).map(s => lData._1 + "#" + s)
    }
  }
  ptData.flatten.flatten
}

// drop entries with empty columns:
val goodData = csv(listData).filterNot(_.contains("##"))
val ptolemyOpts = for (ln <- goodData) yield {
  try {
    val parsed = parseCsv(ln)
    Some(parsed)
  } catch {
    case t: Throwable => {
      println("FAILED ON " + ln)
      None
    }
  }
}

val ptolemy = ptolemyOpts.flatten

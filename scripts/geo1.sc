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
    case _ => continentOpt.get.text
  }
  val chaps = bk \ "div"
  for (ch <- chaps) yield {
    val chNum = ch.attribute("n").get

    val provinceOpt =   ch.attribute("ana")
    val province = provinceOpt match {
      case None => ""
      case _ => provinceOpt.get.text
    }

    val sects = ch \ "div"
    for (sect <- sects) yield {
      val sectNum = sect.attribute("n").get
      val geoType = sect.attribute("type").getOrElse("")

      val props = if (province.isEmpty) { "" } else {Vector(continent, province, geoType).mkString(",") }
      val summary = Vector(bkNum, chNum, sectNum).mkString(".") + "," + props
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
  text: String
)

def parseCsv(s: String): PtolStrings = {
  val cols = s.split(",")
  //println("Parsing " + cols.toVector)
  PtolStrings(
    cols(0),
    cols(1),
    cols(2),
    cols(3),
    cols(4),
    cols(5)
  )
}

def itemProcess(n: scala.xml.Node) : String = {
  val nameSeq = n \ "name"
/*
  <measure  type='llpair'>
      <num  type='cardinal'>κη</num>
      <num  type='fraction'>𐅷 </num>
      <num  type='cardinal'>μβ</num>
      <num  type='fraction'>𐅷 </num>
  </measure>
  */
  if(nameSeq.toVector.nonEmpty) {


    val nd = nameSeq.head
    val processed = nd.attribute("type").get.text match {
      case "episemos" => "" //println("Episemos")
      case "place" => {
        val measures = (n \ "measure" \ "num").toVector
        val id = nd.attribute("key").get.text
        val res = id + "," + nd.text.replaceAll("[\\s]+", " ")
        if (measures.size != 4) {
          println(s"${measures.size} nums in  " + res)
        }
        res
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
  //println("Items: " + processed)
  processed
}


def csv = {
  val ptData = for (lData <- listData) yield {
    //println(lData._1 + " " + lData._2.size)
    for (l <- lData._2) yield {
      listProcess(l).map(s => lData._1 + "," + s)
    }
  }
  ptData.flatten.flatten
}

val goodData = csv.filterNot(_.contains(",,"))
val ptolemy = for (ln <- goodData) yield { parseCsv(ln) }

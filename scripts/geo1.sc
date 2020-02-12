import scala.xml._

val xml = "tei/tlg0363.tlg009.epist03-p5-u8.xml"


val root = XML.loadFile(xml)
val books = root \ "text" \ "body" \ "div"


val lists = for (bk <- books) yield {
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

      val props = if (province.isEmpty) { "" } else {":  " + Vector(continent, province, geoType).mkString(",") }
      val summary = Vector(bkNum, chNum, sectNum).mkString(".") +  props
      val lists =  sect \ "list"
      (summary, lists.toVector)
    }
  }
}

def itemProcess(n: scala.xml.Node) = {
  val nameSeq = n \ "name"
}

val listData = lists.flatten.flatten.toVector.filter(_._2.nonEmpty)



def itemProcess(n: scala.xml.Node) : String = {
  val nameSeq = n \ "name"

  if(nameSeq.toVector.nonEmpty) {
    val nd = nameSeq.head
    val processed = nd.attribute("type").get.text match {
      case "episemos" => "" //println("Episemos")
      case "place" => {
        val id = nd.attribute("key").get.text
        val res = id + " " + nd.text.replaceAll("[\\s]+", " ")
        println(res)
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


def runit = {
  val ptData = for (lData <- listData) yield {
    //println(lData._1 + " " + lData._2.size)
    for (l <- lData._2) yield {
      listProcess(l)
    }
  }
  ptData.flatten.flatten
}

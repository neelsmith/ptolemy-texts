import scala.xml._

val f = "tei/tlg0363.tlg009.epist03-p5-u8.xml"

val root = XML.loadFile(f)
val books = (root \ "text" \ "body" \ "div").toVector

def bookXml(bk: String) : scala.xml.Node = {
  val bks = books.filter(_.attributes("n").text == bk)
  if (bks.size !=1) {
    throw new Exception("No, no,no.")
  } else {
    bks(0)
  }
}

def twoTierXml(bk: String, chap: String) : scala.xml.Node= {
  val book = bookXml(bk)
  val chapts = (book \ "div").toVector.filter(_.attributes("n").text == chap)
  if (chapts.size != 1) {
    throw new Exception("Again, no, no,no.")
  } else {
    chapts(0)
  }
}


def threeTierXml(bk: String, chap: String, sect: String): scala.xml.Node = {
  val chapter = twoTierXml(bk, chap)
  val sects = (chapter \ "div").toVector.filter(_.attributes("n").text == sect)
  if (sects.size != 1) {
    throw new Exception("Again and again, no, no, no: " + sects.size + " sections!")
  } else {
    sects(0)
  }
}

def listItems(ref: String) = {
  val parts = ref.split("\\.")
  val section = threeTierXml(parts(0),parts(1),parts(2))
  val items = section \ "list" \ "item"
  items.toVector
}


def walkGeo(geo: scala.xml.Node, context: String, ethnic: String) {
  val geoId = geo.attributes("n").text
  val ref = context + "." + geoId
  val typeAttr = geo.attributes("type")
  val geoType = if (typeAttr == null) {
    "NO GEO TYPE"
  } else if (typeAttr.text == "section") {
    "text paragraph"
  } else {
    typeAttr.text
  }

  val items = listItems(ref)
  println( ref + " (" + List(ethnic, geoType).mkString(", ") + ") "+ items.size + " items." )
}
def walkEthnics(ethnic: scala.xml.Node, bookNum: String, continent: String) {
  val ethnicId = ethnic.attributes("n").text
  val ethnicAttr = ethnic.attributes("ana")
  val ethnicType = if (ethnicAttr == null){
    "NO ETHNIC"
  } else {
    ethnicAttr.text
  }
  val ref = bookNum + "." + ethnicId

  val geos = (ethnic \ "div").toVector
  for (geo <- geos) {
    walkGeo(geo, ref, Vector(continent, ethnicType).mkString(", "))
  }
}
def walkBook(bookXml : scala.xml.Node) = {
  val bkNum = bookXml.attributes("n").text
  val continentAttr = bookXml.attributes("ana")
  val continent = if (continentAttr == null) {"NO CONTINENT"} else { continentAttr.text }
  val ethnics = (bookXml \ "div").toVector
  for (ethnic <- ethnics) {
    walkEthnics(ethnic, bkNum, continent)
  }

}
def walkTree(startBook: Int = 2, endBook: Int = 8) = {
  // Book 1 has no data, so start with second book, ie., books(1)
  // Book 8 has data, but only for submaps.
  for (bk <- (startBook - 1) to (endBook - 1)) {
    val bookXml = books(bk)
    walkBook(bookXml)
  }
}
/*
val ethnics = (root \ "text" \ "body" \ "div" \ "div").toVector
val geoareas = (root \ "text" \ "body" \ "div" \ "div" \ "div").toVector

val rawLists = for (ga <- geoareas) yield {
  val lllists = (ga \ "list").toVector
  lllists
}
val ptLists = rawLists.flatten



val ptNodes = for (ptList <- ptLists) yield {

  ptList \ "item"
}




val sectiontypes = for (ga <- geoareas) yield {
  ga.attributes("type").text
}



case class GreekGeoPoint (label : String, id: String, lonDegree: String, lonFraction: String, latDegree: String, latFraction: String) {
}

def itemToGeoPoint(itemNode: scala.xml.Node): Option[GreekGeoPoint] = {
  val pointNameList =  (itemNode \ "name").toVector
  val digits = (itemNode \ "measure" \ "num").toVector
  if (pointNameList.size != 1) {
    println("WRONG NUMBER OF NAMES ELEMENT in " + itemNode)
    None

  } else if (digits.size != 4) {
    println("Wrong number of measurements in " + itemNode)
    None

  } else {
    val ptName = pointNameList(0)
    val ptId = ptName.attributes("key").text

    Some(GreekGeoPoint(ptName.text,ptId,digits(0).text,digits(1).text,digits(2).text,digits(3).text)  )
  }
}


val pts = ptNodes.toVector.flatten

val measureNodes = for (pt <- pts) yield {
  (pt \ "measure").toVector
}
val measures = measureNodes.toVector.flatten


val mTypes = for ((m,i) <- measures.zipWithIndex) yield {
  println(i + ". " + m.text)

  val t = m.attributes("type")

  println(t)
  if (t == null) {
    "NO TYPE ATTRIBUTE on " + m
  } else {
    t.text
  }
}

val typesGrouped = mTypes.filterNot(_.contains("NO TYPE")).groupBy(t => t)
val typesHist = typesGrouped.map{ case (k,v) => (k,v.size) }
val typesOrdered = typesHist.toVector.sortBy(_._2).reverse

*/



/*
val gkPts = for (pt <- pts) yield {
  //println("\n\n" + pt.text)
  val pointNameList =  (pt \ "name").toVector
  val digits = (pt \ "measure" \ "num").toVector
  if (pointNameList.size != 1) {
    "WRONG NUMBER OF NAMES ELEMENT in " + pt

  } else {
    //pointNameList(0).attributes("name").text
    val ptName = pointNameList(0)
    println("One name in " + ptName)
    ptName.text
  }
}
  //val gkPt = itemToGeoPoint(pt)
  //println(gkPt)
  //gkPt
  */

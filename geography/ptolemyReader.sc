import scala.xml._

val f = "tei/tlg0363.tlg009.epist03-p5-u8.xml"

val root = XML.loadFile(f)
val books = (root \ "text" \ "body" \ "div").toVector
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

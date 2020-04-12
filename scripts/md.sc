import scala.xml._

val xmlFile = "tei/tlg0363.tlg009.epist03-p5-u8.xml"


val root = XML.loadFile(xmlFile)
val books = root \ "text" \ "body" \ "div"


// recursively parse
def mdForNode(n : scala.xml.Node, accum: String = "") : String = {
  val txt = n match {
    case t: xml.Text => {
      t.text
    }
    case e: xml.Elem => {
      val childText = for (ch <- e.child) yield {
        mdForNode(ch, accum)
      }
      childText.mkString("")
    }
  }
  txt
}

val listsOLists = for (bk <- books) yield {
  val bkNum = bk.attribute("n").get

  val chaps = bk \ "div"
  val inherited = for (ch <- chaps) yield {
    val chNum = ch.attribute("n").get

    val sects = ch \ "div"
    val reff = for (sect <- sects) yield {
      val sectNum = sect.attribute("n").get
      val ref  = Vector(bkNum, chNum, sectNum).mkString(".")

      (ref, mdForNode(sect))
    }
    reff
  }
  inherited.flatten
}
val psgs = listsOLists.flatten.toVector

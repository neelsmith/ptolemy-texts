
import edu.holycross.shot.xmlutils._
import scala.xml._

val f = "ptolemy-raw/almagest-valid-tei.xml"
val root = XML.load(f)
val body = (root \\ "body").toVector.head

val bks = (body \ "div").toVector

val cexVectors = for (bk <- bks) yield {
  val bkNum = (bk.attribute("n").get.text)
  val chaps = (bk \ "div").toVector
  for (chap <- chaps) yield {
    val chapNum = (chap.attribute("n").get.text)

    val head = (chap \ "head").toVector
    val hdrCex = if (head.isEmpty) {
      bkNum + "." + chapNum + ".toc#"
    }else {
      bkNum + "." + chapNum + ".title#" + head.text.replaceAll("[ \n]+", " ")
    }
    //println(hdrCex)
    val paras = (chap \ "p").toVector.zipWithIndex
    for (p <- paras) yield {
      val pNum = p._2 + 1
      val pRef = bkNum + "." + chapNum + "." + pNum + "#"
      pRef + p._1.text.replaceAll("[ \n]+", " ")
    }
  }
}

// Remove tiering in 3 hierarchies:
val cex = cexVectors.flatten.flatten

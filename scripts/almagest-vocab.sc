
//import edu.holycross.shot.xmlutils._
import scala.xml._

//val f = "ptolemy-raw/almagest-valid-tei.xml"
val f = "ptolemy-raw/almagest-no-catalog.xml"
val root = XML.load(f)
val body = (root \\ "body").toVector.head


// Collect all text content except for <num> elements.
def collectText(n: xml.Node, s: String = ""): String = {
  var txt = s
  n match {
    case t: xml.Text =>  {
      val cleaner = t.toString().trim
      if (cleaner.nonEmpty){
        txt += cleaner + " "
      }
    }
    case e: xml.Elem =>  {

      e.label match {
        case "num" => "" //
        case "rs" => "" //
        case _ => {
          for (ch <- e.child) {
            txt += collectText(ch, s)
          }
        }
      }
   }
  }
  txt
}


val txt = collectText(body)
val words =   txt.replaceAll("\n"," ").split("[ ]+").toVector.map(_.replaceAll("[\\.;:,]","")).filter(_.nonEmpty)

val wordsLC = words.map(_.toLowerCase).distinct


import java.io.PrintWriter
def writeCounts(wordList: Vector[String], outFile: String = "almagest-vocab.txt") {
  val grouped = wordList.groupBy(w => w)
  val counts = grouped.map{ case (wd, v) => (wd, v.size)}
  val countsVect = counts.toVector.sortBy(_._2).reverse
  val strs = countsVect.map{ case (wd,count) => wd + "#" + count}
  new PrintWriter(outFile){write(strs.mkString("\n"));close;}
}

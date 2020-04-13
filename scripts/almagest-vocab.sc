
import edu.holycross.shot.xmlutils._
import scala.xml._

val f = "ptolemy-raw/almagest-valid-tei.xml"
val root = XML.load(f)
val body = (root \\ "body").toVector.head
//val txt = TextReader.collectText(body)



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

      if (e.label == "num") {
          // skip
      } else {
        for (ch <- e.child) {
         txt += collectText(ch, s)
       }
      }
   }
  }
  txt
}


val txt = collectText(body)
val words = txt.replaceAll("[\\.;:,\n]","").split("[ ]+").filter(_.nonEmpty).map(_.toLowerCase).toVector

import java.io.PrintWriter
def writeCounts(wordList: Vector[String]) {
  val grouped = wordList.groupBy(w => w)
  val counts = grouped.map{ case (wd, v) => (wd, v.size)}
  val countsVect = counts.toVector.sortBy(_._2).reverse
  val strs = countsVect.map{ case (wd,count) => wd + "#" + count}
  new PrintWriter("almagest-vocab.txt"){write(strs.mkString("\n"));close;}
}


import edu.holycross.shot.xmlutils._
import scala.xml._

val f = "ptolemy-raw/almagest-valid-tei.xml"
val root = XML.load(f)
val body = (root \\ "body").toVector.head
val txt = TextReader.collectText(body)

val words = txt.replaceAll("[\\.;:,\n]","").split("[ ]+").filter(_.nonEmpty).map(_.toLowerCase)

val grouped = words.groupBy(w => w)
val counts = grouped.map{ case (wd, v) => (wd, v.size)}
val countsVect = counts.toVector.sortBy(_._2).reverse

val strs = countsVect.map{ case (wd,count) => wd + "#" + count}

import java.io.PrintWriter
new PrintWriter("almagest-vocab.txt"){write(strs.mkString("\n"));close;}

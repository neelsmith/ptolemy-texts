
import scala.io._
val f = "ocr/harmonics.cex"

val txtLines = Source.fromFile(f).getLines.toVector

val txt = txtLines.map (ln => {
  val cols = ln.split("#")
  cols(1)
}
).mkString(" ")



val words = txt.split("[ ]+").toVector.map(_.toLowerCase).map(_.replaceAll("[\\.;:,]","")).filter(_.nonEmpty)



import java.io.PrintWriter
def writeCounts(wordList: Vector[String]) {
  val grouped = wordList.groupBy(w => w)
  val counts = grouped.map{ case (wd, v) => (wd, v.size)}
  val countsVect = counts.toVector.sortBy(_._2).reverse
  val strs = countsVect.map{ case (wd,count) => wd + "#" + count}
  new PrintWriter("harmonics-vocab.txt"){write(strs.mkString("\n"));close;}
}

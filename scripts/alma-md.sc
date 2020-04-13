//read CEX, write jekyll pages

import scala.io._
import edu.holycross.shot.cite._
import java.io.PrintWriter


val targetDir = "md/almagest/"
val srcFile = "ocr/almagest.cex"


val lines = Source.fromFile(srcFile).getLines.toVector

for (ln <- lines.zipWithIndex) {
  val idx = ln._2
  val prev = if (idx == 0) {""} else {
    val prevCols = lines(idx - 1).split("#")
    val prevUrn = CtsUrn(prevCols(0))
    s"[${prevUrn.passageComponent}](../${prevUrn.passageComponent}/)"
  }

  val next = if (idx == lines.size - 1) {""} else {
    val nextCols = lines(idx + 1).split("#")
    val nextUrn = CtsUrn(nextCols(0))
    s"[${nextUrn.passageComponent}](../${nextUrn.passageComponent}/)"
  }

  val cols = ln._1.split("#")

  if (cols.size != 2 ) {
    println("ERROR ON LINE " + ln)
  } else {
    val u = CtsUrn(cols(0))
    val fName = targetDir + u.passageComponent + ".md"

    val title = "Ptolemy, Almagest, " + u.passageComponent
    val hdr = "---\ntitle: " + title + "\nlayout: page\n---\n\n"
    val txt = cols(1)

    val nav = s"\n\n---\n\nprev: ${prev} | next: ${next}\n\n"
    new PrintWriter(fName){write(hdr + txt + nav);close;}
  }
}

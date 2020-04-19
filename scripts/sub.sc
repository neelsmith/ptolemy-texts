import scala.io.Source
import java.io.PrintWriter

val txtFile = "ptolemy-raw/almagest-valid-tei.xml"
val text = Source.fromFile(txtFile).getLines.toVector.mkString("\n")

//val subFile = "patches/alma-uppercase-singletons.txt"
//val subFile = "patches/uppersForRS.txt"
//val subFile = "patches/uppersWNL.txt"
val subFile = "patches/alma-singletons.txt"

val subLines = Source.fromFile(subFile).getLines.toVector
val pairs = subLines.map( l => l.split("#").toVector)


def substitute(subs: Vector[Vector[String]], txt: String) : String = {
  if (subs.isEmpty) {
    txt
  } else {
    val pair = subs.head
    val newText = txt.replaceAll(pair(0), pair(1))
    substitute(subs.tail, newText)
  }
}

// substitute(pairs, text)

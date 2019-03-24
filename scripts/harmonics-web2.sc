import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import java.io.File
import java.io.PrintWriter

///////////////////////////////////////////////////////////////////////////
//
// For the digital version of Düring's edition of the *Harmonics*,
// generate a series of markdown files with suitable yaml header to
// drop into a jekyll web service.  Content is organized with one file per
// level-2 citation. Files should:
//
// 1. have file names reflecting canonical passage ID
// 2. include navigational links to previous/next nodes in the text
//
// Usage:
//     formatCorpus2()
//
///////////////////////////////////////////////////////////////////////////


val harmonicsFile = "ocr/harmonics.cex"
val corpus = CorpusSource.fromFile(harmonicsFile)
val twotierUrns = corpus.nodes.map(_.urn.collapsePassageBy(1)).distinct

def prevLink(u: CtsUrn, c: Corpus) : String = {
  c.prevUrn(u) match {
    case None => ""
    case prv: Option[CtsUrn] => {
      val collapsed = if (prv.get.isRange) {
        CtsUrn(prv.get.dropPassage.toString + prv.get.rangeBegin).collapsePassageTo(2)
      } else {
        prv.get.collapsePassageTo(2)
      }
      //println("COLLAPSED: " + collapsed)

      s"[${collapsed.passageComponent}](../${collapsed.passageComponent}/)"
    }
  }
}

def nextLink(u: CtsUrn, c: Corpus) : String = {
  //println("FIND NEXTLINK FOR " + u)
  c.nextUrn(u) match {
    case None => ""
    case nxt: Option[CtsUrn] => {
      val collapsed = if (nxt.get.isRange) {
        CtsUrn(nxt.get.dropPassage.toString + nxt.get.rangeBegin).collapsePassageTo(2)
      } else {
        nxt.get.collapsePassageTo(2)
      }
      //println("COLLAPSED: " + collapsed)

      s"[${collapsed.passageComponent}](../${collapsed.passageComponent}/)"
    }
  }
}



def formatNode2(nodes: Vector[CitableNode], c: Corpus): String = {
  val urn = nodes(0).urn.collapsePassageTo(2)
  println("LOOK AT " + urn)
  val yaml = s"---\ntitle: Ptolemy Harmonics ${urn.passageComponent}\nlayout: page\n---\n\n"

  val prevString = c.prevUrn(urn) match {
    case None => ""
    case prv: Option[CtsUrn] =>  prevLink(urn,c)
  }
  val nextString = c.nextUrn(urn) match {
    case None => ""
    case prv: Option[CtsUrn] => nextLink(urn,c)
  }
  val links = s"${prevString} | ${nextString} "

  val mdNodes = for (nd <- nodes) yield  {
    println("Format " + nd.urn.passageComponent)
    nd match {
      case title if title.urn.toString.endsWith("title") =>  "*" + title.text + "*" + "\n"
      case txt if txt.urn.toString.endsWith("text") => txt.text
      case tocEntry if tocEntry.urn.toString.contains(".toc.") => "-  " + tocEntry.text
    }
  }
  val text = mdNodes.mkString("\n")

  yaml + "\n\n" + text + "\n\n" + links + "\n\n"
}



def formatCorpus2(c: Corpus = corpus, dir: String = "harmonics", urns: Vector[CtsUrn] = twotierUrns) = {
  val workDir = new File(dir)
  if (! workDir.exists) {workDir.mkdir()}

  for (u <- urns) {
    println(u)
    val subCorp = c ~~ u
    println(">NODES" + subCorp.nodes.map(_.urn.passageComponent))

    val pgDir = new File(dir + "/" + u.passageComponent)
    if (! pgDir.exists) {pgDir.mkdir()}
    val outFile = new File(pgDir, "index.md")


    val pgText = formatNode2(subCorp.nodes,c)
    new PrintWriter(outFile) { write (pgText); close;}

  }
}

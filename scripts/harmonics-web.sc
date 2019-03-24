import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import java.io.File
import java.io.PrintWriter

///////////////////////////////////////////////////////////////////////////
//
// For the digital version of Düring's edition of the *Harmonics*,
// generate a series of markdown files with suitable yaml header to
// drop into a jekyll web service, one file per citable node.  Files should:
//
// 1. have file names reflecting canonical passage ID
// 2. include navigational links to previous/next nodes in the text
//
// Usage:
//     formatCorpus()
//
///////////////////////////////////////////////////////////////////////////

val harmonicsFile = "ocr/harmonics.cex"
val corpus = CorpusSource.fromFile(harmonicsFile)


def prevLink(u: CtsUrn, c: Corpus) : String = {
  c.prevUrn(u) match {
    case None => ""
    case prv: Option[CtsUrn] => s"[${prv.get.passageComponent}](../${prv.get.passageComponent}/)"
  }
}

def nextLink(u: CtsUrn, c: Corpus) : String = {
  c.nextUrn(u) match {
    case None => ""
    case prv: Option[CtsUrn] => s"[${prv.get.passageComponent}](../${prv.get.passageComponent}/)"
  }
}


def formatNode(n: CitableNode, c: Corpus): String = {
  val yaml = s"---\ntitle: Ptolemy Harmonics ${n.urn.passageComponent}\nlayout: page\n---\n\n"

  val prevString = c.prevUrn(n.urn) match {
    case None => ""
    case prv: Option[CtsUrn] =>  prevLink(n.urn,c)
  }
  val nextString = c.nextUrn(n.urn) match {
    case None => ""
    case prv: Option[CtsUrn] => nextLink(n.urn,c)
  }
  val links = s"${prevString} | ${nextString} "

  val text = if (n.urn.toString.endsWith(".title")) {
    "*" + n.text + "*"
  } else {
    n.text
  }
  yaml + "\n\n" + text + "\n\n" + links + "\n\n"
}

//
// Format corpus of Ptolemy's *Harmonics* as markdown, and write
// a series of linked markdown files.
//
def formatCorpus(c: Corpus = corpus, dir: String = "harmonics") = {
  val workDir = new File(dir)
  if (! workDir.exists) {workDir.mkdir()}
  for (n <- c.nodes) {
    val pgDir = new File(dir + "/" + n.urn.passageComponent)
    if (! pgDir.exists) {pgDir.mkdir()}
    val outFile = new File(pgDir, "index.md")
    val pgText = formatNode(n,c)
    new PrintWriter(outFile) { write (pgText); close;}
  }
}

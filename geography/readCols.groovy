
File f = new File(args[0])

Integer cnt = 0

println "BernID\tSiteName\tPsg\tLatDeg\tLatFract\tLonDeg\tLonFract"
f.eachLine { ln ->
  def cols = ln.split(/\t/)
  String siteName = cols[0]
  String ref = cols[1]
  String modernName = cols[2]
  String latStr = cols[4]
  String lonStr = cols[5]

  if (latStr != "") {
    def latCols = latStr.split(/[ ]+/)
    String latDeg = latCols[0]
    String latFract = ""
    if (latCols.size() > 1) {
      latFract = latCols[1]
    }


    def lonCols = lonStr.split(/[ ]+/)
    String lonDeg = lonCols[0]
    String lonFract = ""
    if (lonCols.size() > 1) {
      lonFract = lonCols[1]
    }


    def pts = ref.split(/\./)
    String chap = pts[1]
    String sect = pts[2]
    String item = pts[3]
    String urnRef = pts[0]+ "." + chap.replaceFirst(/^0/,"") + "." + sect.replaceFirst(/^0/,"") 
    
    println "${ref}\t${siteName}\t${urnRef}\t${latDeg.replaceAll(/[^\d]/,'')}\t${latFract.replaceAll(/[^\d]/,'')}\t${lonDeg.replaceAll(/[^\d]/,'')}\t${lonFract.replaceAll(/[^\d]/,'')}"

  }

  cnt++;
}
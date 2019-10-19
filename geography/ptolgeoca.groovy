import edu.harvard.chs.f1k.GreekNode


// Make a derived exemplar by list elements.

// script to write a suite of collection records from 
// edition of Ptolemy, Geography
// citation on @n attrs, data on @ana
/*
TEI/text/body/div/div/div/list
 */

groovy.xml.Namespace tei = new groovy.xml.Namespace("http://www.tei-c.org/ns/1.0")


String urnBase = "urn:cite:pm:geo"
Integer recordCount = 0
Integer listCount = 0
String ctsUrnBase = "urn:cts:greekLit:tlg0363.tlg009.hc"


File f = new File(args[0])
groovy.util.Node root = new XmlParser().parse(f)
String psg = ""

root[tei.text][tei.body][tei.div].each { book ->
  String bookRef = book.'@n'
  String continent = book.'@ana'
  //println psg + ": " + continent
  book[tei.div].each { chapt ->
    
    String chaptRef = bookRef + "." + chapt.'@n'
    String satrapy = chapt.'@ana'

    chapt[tei.div].each { sect ->
      Integer listNum = 0
      String geoType = sect.'@type'
      psg = ctsUrnBase + ":" + chaptRef + "." + sect.'@n'
      sect[tei.list].each { lst ->
	listCount++;
	listNum++;
	psg = psg + ".${listNum}"
	//println "${psg}: ${listCount}"
	sect[tei.list][tei.item].each { rec ->
	  recordCount++;
	  String placeNameId
	  String placeNameText
	  rec[tei.name].each { toponym ->
	    placeNameId = toponym.'@key'
	    GreekNode n = new GreekNode(toponym)
	    placeNameText = n.collectText()
	  }
	  
	  println "${psg},${urnBase}.${recordCount},${continent},${satrapy},${geoType},ETHNIC,${placeNameId},${placeNameText}"
	}

      }
    }
      /*
      // test if there is either a placeName or 
      sect[tei.ab].each { ln ->
	if ((ln[tei.placeName].size() > 0) || (ln[tei.measure].size() > 0)) {
	  print "${ctsUrnBase}:${psg},"

	  GreekNode n = new GreekNode(ln)
	  txt = n.collectText()
	  // ?? how does this regexp replace work?
	  //txt = txt.replaceAll("~/\t\n/", "")
	  //txt = txt.replaceFirst("~/.+/","")
	  print "${txt},"


	  recordCount++;
	  print "${urnBase}.${recordCount},"
	  psg = sectId + "." + ln.'@n'

	  ln[tei.placeName].each { pl ->
	    if (pl.'@n') {
	      print pl.'@n'
	    }
	  }
	  print ","
	  ln[tei.measure][tei.num].each { num ->
	    if (num.'@value') {
	      print num.'@value'
	    }
	  }
	  println ""
    */
  }
}

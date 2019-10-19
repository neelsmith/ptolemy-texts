package edu.holycross.shot.ptolemy


/**
*/
sealed trait Continent {
  def name: String
  /// def urn ...
}

object Continent  {
  def apply(s: String) = {
    s.toLowerCase match {
      case "europe" => Europe
      case "asia" => Asia
      case "africa" => Africa
      case _ => throw new Exception("Unrecognized name for continent: " + s)
    }
  }
}

/**  */
case object Europe extends Continent {
  def name = "Europe"
}
case object Asia extends Continent {
  def name = "Asia"
}
case object Africa extends Continent {
  def name = "Africa"
}

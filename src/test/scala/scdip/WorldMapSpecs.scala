package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class WorldMapSpecs extends Specification {
  "WorldMap" >> {
    val worldMap = WorldMap.fromElem(XML.load(getClass.getResourceAsStream("/std_adjacency.xml")))
    "size" >> {
      worldMap.size === 76
    }
    "WorldMap#province" >> {
      worldMap.province("London").shortName === "lon"
    }
    "WorldMap#location" >> {
      worldMap.location("nth-mv") must throwA[RuntimeException]
    }
    "neighbour" >> {
      val mosMv = worldMap.location("mos-mv")
      val ukrMv = worldMap.location("ukr-mv")
      val lonMv = worldMap.location("lon-mv")
      val nthXc = worldMap.location("nth-xc")
      val lvpMv = worldMap.location("lvp-mv")
      val iriMv = Location(worldMap.province("iri"), Option(Coast.Land))
      worldMap.isNeighbour(mosMv, ukrMv) must beTrue
      worldMap.isNeighbour(mosMv, lonMv) must beFalse
      worldMap.isNeighbour(nthXc, lonMv) must beFalse
      worldMap.isNeighbour(lvpMv, iriMv) must beFalse
    }
    "isReachable" >> {
      val midXc = worldMap.location("mid-xc")
      val gasMv = worldMap.location("gas-mv")
      val spaNc = worldMap.location("spa-nc")
      val spaSc = worldMap.location("spa-sc")
      val spaMv = worldMap.location("spa-mv")
      val botXc = worldMap.location("bot-xc")
      val stpNc = worldMap.location("stp-nc")
      val stpMv = worldMap.location("stp-mv")

      worldMap.isReachable(midXc, gasMv) must beTrue
      worldMap.isReachable(gasMv, midXc) must beFalse
      worldMap.isReachable(midXc, spaNc) must beTrue
      worldMap.isReachable(midXc, spaSc) must beTrue
      worldMap.isReachable(midXc, spaMv) must beTrue

      worldMap.isReachable(spaNc, midXc) must beTrue
      worldMap.isReachable(spaSc, midXc) must beTrue
      worldMap.isReachable(spaMv, midXc) must beFalse

      worldMap.isReachable(botXc, stpNc) must beTrue
      worldMap.isReachable(stpNc, botXc) must beFalse
      worldMap.isReachable(botXc, stpMv) must beTrue

    }
    "convoy" >> {
      val lon = worldMap.province("lon")
      val syr = worldMap.province("syr")
      val mos = worldMap.province("mos")
      val nor = worldMap.province("nor")
      val lvn = worldMap.province("lvn")
      val lvp = worldMap.province("lvp")
      val iri = worldMap.province("iri")
      worldMap.canConvoy(lon, nor) must beTrue
      worldMap.canConvoy(lon, syr) must beTrue
      worldMap.canConvoy(mos, nor) must beFalse
      worldMap.canConvoy(lon, lvn) must beTrue
      worldMap.canConvoy(lvp, iri) must beFalse
    }
  }

}

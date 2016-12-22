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
      worldMap.isNeighbour(mosMv, ukrMv) must beTrue
      worldMap.isNeighbour(mosMv, lonMv) must beFalse
      worldMap.isNeighbour(nthXc, lonMv) must beFalse
    }
    "convoy" >> {
      val lon = worldMap.province("lon")
      val syr = worldMap.province("syr")
      val mos = worldMap.province("mos")
      val nor = worldMap.province("nor")
      val lvn = worldMap.province("lvn")
      worldMap.canConvoy(lon, nor) must beTrue
      worldMap.canConvoy(lon, syr) must beTrue
      worldMap.canConvoy(mos, nor) must beFalse
      worldMap.canConvoy(lon, lvn) must beTrue
    }
  }

}

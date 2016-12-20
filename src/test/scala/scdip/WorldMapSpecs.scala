package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class WorldMapSpecs extends Specification {
  "WorldMap" >> {
    val stream = getClass.getResourceAsStream("/std_adjacency.xml")
    val tree = XML.load(stream)
    val worldMap = WorldMap(tree)
    worldMap.provinces must have size 76
    worldMap.province("London").shortName === "lon"
    val mosMv = worldMap.location("mos-mv")
    val ukrMv = worldMap.location("ukr-mv")
    val lonMv = worldMap.location("lon-mv")
    worldMap.isNeighbour(mosMv, ukrMv) must beTrue
    worldMap.isNeighbour(mosMv, lonMv) must beFalse
    worldMap.canConvoy(worldMap.province("lon"), worldMap.province("nor")) must beTrue
    worldMap.canConvoy(worldMap.province("lon"), worldMap.province("syr")) must beTrue
    worldMap.canConvoy(worldMap.province("mos"), worldMap.province("nor")) must beFalse
    worldMap.canConvoy(worldMap.province("lon"), worldMap.province("lvn")) must beFalse
  }

}

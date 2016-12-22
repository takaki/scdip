package scdip

import scdip.Coast.SeaCoast

import scala.collection.immutable.{Seq, TreeMap}
import scala.xml.{Elem, NodeSeq}
import scalax.collection.Graph
import scalax.collection.GraphEdge._

object Coast {

  trait SeaCoast extends Coast {
    // TODO: FIX ME
    override val defaultCoast = Single
    override val isSea: Boolean = true
  }

  case object Undefined extends Coast {
    override def defaultCoast: Coast = Undefined

    val abbreviation: String = "?"
  }

  case object Wing extends Coast {
    override val defaultCoast: Coast = Wing
    val abbreviation = "wx"
  }

  case object Land extends Coast {
    override def defaultCoast: Coast = Land

    override val isLand = true
    val abbreviation: String = "mv"
  }

  case object Single extends SeaCoast {
    val abbreviation: String = "xc"
  }

  case object North extends SeaCoast {
    val abbreviation: String = "nc"
  }

  case object South extends SeaCoast {
    val abbreviation: String = "sc"
  }

  case object West extends SeaCoast {
    val abbreviation: String = "wc"
  }

  case object East extends SeaCoast {
    val abbreviation: String = "ec"
  }

  def parse(input: String): Coast = {
    input match {
      case "wx" => Wing
      case "mv" => Land
      case "xc" => Single
      case "nc" => North
      case "sc" => South
      case "wc" => West
      case "ec" => East
      case _ => Undefined
    }
  }
}

sealed trait Coast {
  def defaultCoast: Coast

  def isSea: Boolean = false

  def isLand: Boolean = false

  def abbreviation: String

  override def toString: String = abbreviation
}


case class Province(fullName: String, shortName: String,
                    supplyCenter: Boolean = false) {
  override def toString: String = shortName
}


case class Location(province: Province, coast: Coast) {
  override def toString: String = s"$province-$coast"

  def sameProvince(loc: Location): Boolean = this.province == loc.province

  def ~~(location: Location): Boolean = sameProvince(location)

  def setCoast(unitType: UnitType): Location = {
    if (coast == Coast.Undefined) copy(coast = unitType.defaultCoast) else this
  }
}


case class WorldMap(provinceMap: Map[String, Province], es: Seq[(Location, Location)]) {
  private val diEdges = es.map(e => DiEdge(e._1, e._2))
  private val graph: Graph[Location, DiEdge] = Graph.from(edges = diEdges)
  private val provinces = diEdges.map(de => de.from.province).distinct

  private val armyProvinces: Set[Province] = diEdges.filter(de => de.from.coast.isLand).map(de => de.from.province).toSet
  private val fleetProvinces: Set[Province] = diEdges.filter(de => de.from.coast.isSea).map(de => de.from.province).toSet

  private val seaProvinces = fleetProvinces -- armyProvinces
  private val landProvinces = armyProvinces -- fleetProvinces
  private val coastalProvinces = armyProvinces & fleetProvinces

  private val armyEdges = diEdges.filter(e => armyProvinces.contains(e.from.province) && e.from.coast.isLand)
  private val fleetEdges = diEdges.filter(e => fleetProvinces.contains(e.from.province) && e.from.coast.isSea)

  private val armyGraph: Graph[Location, DiEdge] = Graph.from(edges = armyEdges)
  private val fleetGraph: Graph[Location, DiEdge] = Graph.from(edges = fleetEdges)

  private def convoyGraph(fleetProvinces: Set[Province]) = {
    val seaOnly = fleetProvinces & seaProvinces
    Graph.from(edges = fleetEdges.filter(e => seaOnly.contains(e.from.province) || seaOnly.contains(e.to.province)))
  }

  private val locationMap: Map[String, Location] = {
    graph.nodes.map(n => n.value).foldLeft(Map.empty[String, Location]) { (m, l) =>
      m.updated(l.toString, l)
    }
  }

  def size: Int = provinces.size

  def province(shortName: String): Province = provinceMap(shortName)

  def location(shortName: String): Location = {
    val s = shortName.split("-", 2)
    locationMap(Location(provinceMap(s(0)), Coast.parse(s(1))).toString)
  }

  def isNeighbour(from: Location, to: Location): Boolean = {
    (for {
      nf <- graph find from
      nt <- graph find to
    } yield nf.inNeighbors(nt)).getOrElse(false)
  }

  def canConvoy(from: Province, to: Province, restrict: Set[Province] = seaProvinces): Boolean = {
    // TODO: province or Locatoin??
    val fromCoast = graph.nodes.filter(n => n.province == from && n.coast.isInstanceOf[SeaCoast]).map(n => DiEdge(Location(n.province, Coast.Land), n.value))
    val toCoast = graph.nodes.filter(n => n.province == to && n.coast.isInstanceOf[SeaCoast]).map(n => DiEdge(n.value, Location(n.province, Coast.Land)))
    val fromEdge = diEdges.filter(e => e.from.province == from && e.from.coast.isInstanceOf[SeaCoast])
    val gSeaPlus = convoyGraph(restrict) ++ fromCoast ++ fromEdge ++ toCoast
    val path = for {
      n0 <- gSeaPlus find Location(from, Coast.Land)
      n1 <- gSeaPlus find Location(to, Coast.Land)
      path <- n0 pathTo n1
    } yield path
    path.isDefined
  }

}

object WorldMap {
  def fromElem(elem: Elem): WorldMap = {
    val provinceNodes: NodeSeq = elem \\ "PROVINCE"

    val provinceMap: Map[String, Province] = provinceNodes.map { e =>
      (Province(e \@ "fullname", e \@ "shortname"), (e \ "UNIQUENAME").map(_ \@ "name"))
    }.foldLeft(TreeMap.empty[String, Province](Ordering.by(_.toLowerCase))) {
      case (m, (p, us)) => us.foldLeft(m)((m, u) => m.updated(u, p)).updated(p.shortName, p).updated(p.fullName, p)
    }
    val edges: Seq[(Location, Location)] =
      provinceNodes.flatMap { p =>
        (p \ "ADJACENCY").flatMap { ad =>
          (ad \@ "refs").split(" ").map(_.split("-")).map { a =>
            (Location(provinceMap(p \@ "shortname"), Coast.parse(ad \@ "type")),
              Location(provinceMap(a(0)), if (a.length == 1) {
                Coast.parse(ad \@ "type").defaultCoast
              } else {
                Coast.parse(a(1))
              }))
          }
        }
      }
    WorldMap(provinceMap, edges)
  }
}

package scdip

import scdip.Coast.ANY_SEA

import scala.collection.immutable.TreeMap
import scala.xml.{Elem, NodeSeq}
import scalax.collection.Graph
import scalax.collection.GraphEdge._

object Coast {

  trait ANY_SEA { // TODO: FIX ME
    def defaultCoast = Single
  }

  case object Undefined extends Coast("?") {
    override def defaultCoast: Coast = Undefined
  }

  case object Wing extends Coast("wx") {
    override def defaultCoast: Coast = Wing
  }

  case object Land extends Coast("mv") {
    override def defaultCoast: Coast = Land
  }

  case object Single extends Coast("xc") with ANY_SEA

  case object North extends Coast("nc") with ANY_SEA

  case object South extends Coast("sc") with ANY_SEA

  case object West extends Coast("wc") with ANY_SEA

  case object East extends Coast("ec") with ANY_SEA

  def parse(abbr: String): Coast = {
    abbr match {
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

sealed abstract class Coast(abbreviation: String) {
  def defaultCoast: Coast

  override def toString: String = abbreviation
}


case class Province(fullName: String, shortName: String,
                    supplyCenter: Boolean = false) {
}

case class Location(province: Province, coast: Coast) {
  override def toString: String = {
    s"${province.shortName}-${coast.toString}"
  }

  def sameProvince(loc: Location): Boolean = this.province == loc.province

  def setCoast(unitType: UnitType): Location = {
    if (coast == Coast.Undefined) copy(coast = unitType.defaultCoast) else this
  }
}


case class WorldMap(elem: Elem) {
  private val provinceNodes: NodeSeq = elem \\ "PROVINCE"

  val provinces: Seq[(Province, Seq[String])] = provinceNodes.map(e => (Province(e \@ "fullname", e \@ "shortname"), (e \ "UNIQUENAME").map(_ \@ "name")))
  private val provinceMap: Map[String, Province] = provinces.foldLeft(TreeMap.empty[String, Province](Ordering.by(_.toLowerCase))) {
    case (m, (p, us)) => us.foldLeft(m)((m, u) => m.updated(u, p)).updated(p.shortName, p).updated(p.fullName, p)
  }
  private val seaProvinces: Seq[Province] = provinceNodes.filter(n =>
    (n \ "ADJACENCY").forall(a => a \@ "type" != "mv")).map(n =>
    n \@ "shortname").map(name => provinceMap(name))
  private val edges: Seq[DiEdge[Location]] = provinceNodes.flatMap(p => (p \ "ADJACENCY").flatMap(ad => {
    (ad \@ "refs").split(" ")
      .map(_.split("-"))
      .map(a => DiEdge(Location(provinceMap(p \@ "shortname"), Coast.parse(ad \@ "type")),
        Location(provinceMap(a(0)), if (a.length == 1) {
          Coast.parse(ad \@ "type").defaultCoast
        } else {
          Coast.parse(a(1))
        })))
  }))
  private val seaEdges = edges.filter(e => seaProvinces.contains(e.from.province))

  private val graph = Graph.from(edges = edges)
  private val seaGraph: Graph[Location, DiEdge] = Graph.from(edges = seaEdges)

  private def convoyableGraph(existence: Seq[Province]) = Graph.from(edges = edges.filter(e => existence.contains(e.from.province)))

  def province(shortName: String): Province = provinceMap(shortName)

  def location(shortName: String): Location = {
    val s = shortName.split("-", 2)
    Location(provinceMap(s(0)), Coast.parse(s(1)))
  }

  def isNeighbour(from: Location, to: Location): Boolean = {
    (for {
      nf <- graph find from
      nt <- graph find to
    } yield nf.inNeighbors(nt)).getOrElse(false)
  }

  def canConvoy(from: Province, to: Province, restrict: Seq[Province] = seaProvinces): Boolean = {
    // TODO: province or Locatoin??
    val fromCoast = graph.nodes.filter(n => n.province == from && n.coast.isInstanceOf[ANY_SEA]).map(n => DiEdge(Location(n.province, Coast.Land), n.value))
    val toCoast = graph.nodes.filter(n => n.province == to && n.coast.isInstanceOf[ANY_SEA]).map(n => DiEdge(n.value, Location(n.province, Coast.Land)))
    val fromEdge = edges.filter(e => e.from.province == from && e.from.coast.isInstanceOf[ANY_SEA])
    val gSeaPlus = convoyableGraph(restrict) ++ fromCoast ++ fromEdge ++ toCoast
    val path = for {
      n0 <- gSeaPlus find Location(from, Coast.Land)
      n1 <- gSeaPlus find Location(to, Coast.Land)
      path <- n0 pathTo n1
    } yield path
    path.isDefined
  }

}

object WorldMap {
}

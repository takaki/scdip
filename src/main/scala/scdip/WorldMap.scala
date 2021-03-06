package scdip

import scdip.UnitType.{Army, Fleet}

import scala.collection.immutable.TreeMap
import scala.xml.{Elem, NodeSeq}
import scalax.collection.Graph
import scalax.collection.GraphEdge._

object Coast {

  trait SeaCoast extends Coast {
    override val defaultCoast: Single.type = Single
    override val isSea: Boolean = true
  }

  case object AnyCoast extends Coast {
    override def defaultCoast: Coast = AnyCoast

    val abbreviation: String = "*"
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

  def parse(input: String): Option[Coast] = {
    input match {
      case "wx" => Option(Wing)
      case "mv" => Option(Land)
      case "xc" => Option(Single)
      case "nc" => Option(North)
      case "sc" => Option(South)
      case "wc" => Option(West)
      case "ec" => Option(East)
      case _ => None
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


case class Province(fullName: String, shortName: String) {
  override def toString: String = shortName
}


case class Location(province: Province, coast: Option[Coast]) {
  override def toString: String = s"$province-${coast.fold("?")(_.toString)}"


  def ~~(location: Location): Boolean = this.province == location.province

  def ~~(province: Province): Boolean = this.province == province

  def ~~:(province: Province): Boolean = this.province == province

  def setCoast(unitType: UnitType): Location = {
    unitType match {
      case Army => copy(coast = Option(Coast.Land))
      case Fleet => coast.fold(copy(coast = Option(Coast.Single)))(_ => this)
    }
  }

  def setDstCoast(t: UnitType, src: Location, worldMap: WorldMap): Location = {
    t match {
      case Army => setCoast(t)
      case Fleet => coast.fold(worldMap.findConnected(src.setCoast(t), this.province).getOrElse(this))(_ => this)
    }
  }

}


case class WorldMap(provinceMap: Map[String, Province], edges: Seq[(Location, Location)]) {


  private val diEdges = edges.map(e => DiEdge(e._1, e._2))
  private val graph: Graph[Location, DiEdge] = Graph.from(edges = diEdges)

  private val distanceGraph: Graph[Location, DiEdge] = graph ++ (for {
    f <- graph.nodes
    t <- graph.nodes if f.value ~~ t.value && f != t
  } yield DiEdge(f.value, t.value)).toList

  private val provinces: Set[Province] = graph.nodes.map(n => n.value.province).toSet

  private val armyProvinces: Set[Province] = graph.nodes.filter(n=>n.value.coast.exists(_.isLand)).map(_.value.province).toSet
  private val fleetProvinces: Set[Province] = graph.nodes.filter(n=>n.value.coast.exists(_.isSea)).map(_.value.province).toSet

  private val seaProvinces = fleetProvinces -- armyProvinces
  //  private val landProvinces = armyProvinces -- fleetProvinces
  private val coastalProvinces = armyProvinces & fleetProvinces

  //  private val armyEdges = diEdges.filter(e => armyProvinces.contains(e.from.province) && e.from.coast.exists(_.isLand))
  //  private val fleetEdges = diEdges.filter(e => fleetProvinces.contains(e.from.province) && e.from.coast.exists(_.isSea))

  //  private val armyGraph: Graph[Location, DiEdge] = Graph.from(edges = armyEdges)
  //  private val fleetGraph: Graph[Location, DiEdge] = Graph.from(edges = fleetEdges)

  private val locationMap: Map[String, Location] = {
    graph.nodes.map(n => n.value.toString -> n.value).toMap
  }

  def size: Int = provinces.size

  def province(shortName: String): Province = provinceMap(shortName)

  def location(shortName: String): Location = {
    val s = shortName.split("-", 2)
    locationMap(Location(provinceMap(s(0)), Coast.parse(s(1))).toString)
  }

  def findConnected(src: Location, dst: Province): Option[Location] = {
    val connected = diEdges.filter(p => p.from == src && p.to.province == dst)
    if (connected.size == 1) {
      Option(connected.head.to)
    } else {
      None
    }
  }

  def isNeighbour(from: Location, to: Location): Boolean = {
    (for {
      nf <- graph find from
      nt <- graph find to
    } yield nf.inNeighbors(nt)).getOrElse(false)
  }

  def isReachable(src: Location, dst: Location): Boolean = {
    graph.edges.exists(e=> e.from == src && e.to ~~ dst)
  }

  def canConvoy(from: Province, to: Province, convoys: Set[Province] = seaProvinces): Boolean = {
    if (coastalProvinces.contains(from) && coastalProvinces.contains(to)) {
      val inFrom = graph.nodes.collect { case n if n.province == from && n.coast.exists(_.isSea) => DiEdge(Location(n.province, Option(Coast.Land)), n.value) }
      val inTo = graph.nodes.collect { case n if n.province == to && n.coast.exists(_.isSea) => DiEdge(n.value, Location(n.province, Option(Coast.Land))) }

      val validSea = convoys & seaProvinces
      val fromCoast = diEdges.filter(e => e.from.province == from && e.from.coast.exists(_.isSea) && validSea.contains(e.to.province))
      val toCoast = diEdges.filter(e => validSea.contains(e.from.province) && e.to.province == to && e.to.coast.exists(_.isSea))
      val convoyEdges = diEdges.filter(e => validSea.contains(e.from.province))

      val gSeaPlus = Graph.from(edges = inFrom ++ fromCoast ++ convoyEdges ++ toCoast ++ inTo)

      val path = for {
        n0 <- gSeaPlus find Location(from, Option(Coast.Land))
        n1 <- gSeaPlus find Location(to, Option(Coast.Land))
        path <- n0 pathTo n1
      } yield path
      path.isDefined
    } else {
      false
    }
  }

  // 6.G.7
  def validConvoy(from: Province, to: Province, convoy: Province): Boolean = {
    if (coastalProvinces.contains(from) && coastalProvinces.contains(to) && seaProvinces(convoy)) {
      val inFrom = graph.nodes.collect { case n if n.province == from && n.coast.exists(_.isSea) => DiEdge(Location(n.province, Option(Coast.Land)), n.value) }
      val inTo = graph.nodes.collect { case n if n.province == to && n.coast.exists(_.isSea) => DiEdge(n.value, Location(n.province, Option(Coast.Land))) }

      val fromCoast = diEdges.filter(e => e.from.province == from && e.from.coast.exists(_.isSea) && seaProvinces.contains(e.to.province))
      val toCoast = diEdges.filter(e => seaProvinces.contains(e.from.province) && e.to.province == to && e.to.coast.exists(_.isSea))
      val convoyEdges = diEdges.filter(e => seaProvinces.contains(e.from.province))

      val gSeaPlus0 = Graph.from(edges = inFrom ++ fromCoast ++ convoyEdges)
      val gSeaPlus1 = Graph.from(edges = convoyEdges ++ toCoast ++ inTo)
      val path0 = for {
        n0 <- gSeaPlus0 find Location(from, Option(Coast.Land))
        n1 <- gSeaPlus0 find Location(convoy, Option(Coast.Single))
        path <- n0 pathTo n1
      } yield path
      val path1 = for {
        n0 <- gSeaPlus1 find Location(convoy, Option(Coast.Single))
        n1 <- gSeaPlus1 find Location(to, Option(Coast.Land))
        path <- n0 pathTo n1
      } yield path
      path0.isDefined && path1.isDefined
    } else {
      false
    }
  }

  def neighbours(origin: Location, ngProvinces: Set[Province] = Set.empty): Set[Location] = {
    diEdges.filter(e => e.from == origin && !ngProvinces.contains(e.to.province)).map(e => e.to).toSet
  }

  def distance(from: Location, to: Location): Int = {
    (for {
      f <- distanceGraph.find(from)
      t <- distanceGraph.find(to)
      path <- f.shortestPathTo(t)
    } yield path.nodes.size).getOrElse(10000)
  }

  def exists(location: Location): Boolean = graph.nodes.contains(location)
}

object WorldMap {
  def fromElem(elem: Elem): WorldMap = {
    val provinceNodes: NodeSeq = elem \\ "PROVINCE"

    val provinceMap: Map[String, Province] = TreeMap.empty[String, Province](Ordering.by(_.toLowerCase)) ++
      provinceNodes.map { e =>
        (Province(e \@ "fullname", e \@ "shortname"), (e \ "UNIQUENAME").map(_ \@ "name"))
      }.flatMap {
        case (p, us) => Seq(p.shortName -> p, p.fullName -> p) ++ us.map(u => u -> p)
      }
    val edges: Seq[(Location, Location)] =
      provinceNodes.flatMap { p =>
        (p \ "ADJACENCY").flatMap { ad =>
          (ad \@ "refs").split(" ").map(_.split("-")).map { a =>
            (Location(provinceMap(p \@ "shortname"), Coast.parse(ad \@ "type")),
              Location(provinceMap(a(0)), if (a.length == 1) {
                Coast.parse(ad \@ "type").map(_.defaultCoast)
              } else {
                Coast.parse(a(1))
              }))
          }
        }
      }
    WorldMap(provinceMap, edges)
  }
}

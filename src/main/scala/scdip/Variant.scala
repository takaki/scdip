package scdip

import scdip.Order.MoveOrder
import scdip.PhaseType.{Adjustment, Movement, Retreat}
import scdip.Season.{Fall, Spring}
import scdip.UnitType.Fleet

import scala.collection.immutable.TreeMap
import scala.util.parsing.combinator.RegexParsers
import scala.xml.{Elem, XML}
import scalaz.Scalaz._


case class VariantList(root: Elem) {
  private val variantElem = root \ "VARIANT"
  val name: String = variantElem \@ "name"
  val version: String = variantElem \@ "version"
  val default: String = variantElem \@ "default"
  val description: String = (variantElem \ "DESCRIPTION").head.text
  val mapDefinitions: Seq[MapDefinition] = (variantElem \ "MAP_DEFINITION").map(elem =>
    MapDefinition(elem \@ "id", elem \@ "title", elem \@ "URI", elem \@ "thumbURI", elem \@ "preferredUnitStyle"))

  val variants: Seq[Variant] = (root \ "VARIANT").map(variantElem => {
    val powerMap: Map[String, Power] = TreeMap.empty[String, Power](Ordering.by(_.toLowerCase)) ++ (variantElem \ "POWER").map { elem =>
      (Power(elem \@ "name", (elem \@ "active").toBoolean, elem \@ "adjective"), Option(elem \@ "altnames").filter(_.nonEmpty))
    }.flatMap {
      case ((p, alt)) => Seq(p.name -> p) ++ alt.map(_ -> p)
    }

    Variant(variantElem \@ "name",
      variantElem \ "MAP" \@ "adjacencyURI",
      (variantElem \ "RULEOPTION").map(elem => elem \@ "name" -> elem \@ "value").toMap,
      powerMap,
      variantElem \ "STARTINGTIME" \@ "turn",
      (variantElem \ "VICTORYCONDITIONS").map(ve => VictoryCondition((ve \ "WINNING_SUPPLY_CENTERS" \@ "value").parseInt.toOption.getOrElse(0),
        (ve \ "YEARS_WITHOUT_SC_CAPTURE" \@ "value").parseInt.toOption.getOrElse(0),
        (ve \ "GAME_LENGTH" \@ "value").parseInt.toOption.getOrElse(0)
      )).headOption.getOrElse(VictoryCondition(0, 0, 0)),
      (variantElem \ "SUPPLYCENTER").map(n => (n \@ "province", n \@ "homepower", n \@ "owner")),
      (variantElem \ "INITIALSTATE").map(n => (n \@ "province", n \@ "power", n \@ "unit", n \@ "unitcoast")))
  })
  private val variantMap = variants.map(v => v.name -> v).toMap

  def variant(name: String): Option[Variant] = variantMap.get(name)
}

case class Variant(name: String,
                   adjacencyURI: String, // TODO: to build WorldMap
                   ruleOptions: Map[String, String],
                   powerMap: Map[String, Power],
                   startingTime: String,
                   victoryCondition: VictoryCondition,
                   supplyCenterInfo: Seq[(String, String, String)],
                   initialState: Seq[(String, String, String, String)]
                  ) {

  val worldMap: WorldMap = WorldMap.fromElem(XML.load(getClass.getResourceAsStream("/std_adjacency.xml"))) // TODO

  def power(name: String): Power = powerMap(name)

  def movementState: MovementState = {
    val provinces: Set[Province] = supplyCenterInfo.map { case (province, homepower, _) => worldMap.province(province) }.toSet
    val home = supplyCenterInfo.collect { case (province, homepower, _) if homepower != "none" =>
      worldMap.province(province) -> power(homepower)
    }.toMap
    val owner = supplyCenterInfo.collect { case (province, _, owner) if owner != "none" =>
      worldMap.province(province) -> power(owner)
    }.toMap

    val map = initialState.map(d => {
      val unitType = UnitType.parse(d._3)
      val coast: Option[Coast] = if (d._4.isEmpty) Option(unitType.defaultCoast) else Coast.parse(d._4)
      val location = Location(worldMap.province(d._1), coast)
      location -> UnitPosition(power(d._2), unitType, location)
    }).toMap

    MovementState(WorldInfo(worldMap, powerMap.values.toSeq, victoryCondition), Phase.parse(startingTime).turn, UnitLocation(map), SupplyCenterInfo(provinces, home, owner))
  }

}

case class VictoryCondition(winningSupplyCenters: Int, yearsWithoutScCapture: Int, gameLength: Int)

case class SupplyCenterInfo(provinces: Set[Province], home: Map[Province, Power], owner: Map[Province, Power]) {


  def ownHomes(power: Power): Set[Province] = home.collect { case (pr, pw) if pw == power => pr }.toSet & owner.collect { case (pr, pw) if pw == power => pr }.toSet

  def isOwner(province: Province, power: Power): Boolean = owner.get(province).contains(power)

  def isHome(province: Province, power: Power): Boolean = home.get(province).contains(power)

  def count(power: Power): Int = owner.values.count(_ == power)

  def updated(province: Province, power: Power): SupplyCenterInfo = {
    if (home.contains(province)) {
      copy(owner = owner.updated(province, power))
    } else {
      this
    }
  }
}

case class UnitLocation(locationUnitMap: Map[Location, UnitPosition]) {
  override def toString: String = locationUnitMap.values.map(_.toString).mkString("; ")

  def ownUnits(power: Power): Seq[UnitPosition] = {
    locationUnitMap.collect { case (_, g) if g.power == power => g }.toSeq
  }


  def exists(unitPosition: UnitPosition): Boolean = locationUnitMap.exists { case (_, g) => g == unitPosition }

  def count(power: Power): Int = locationUnitMap.values.count(_.power == power)

  def isClear(location: Location): Boolean = !locationUnitMap.keys.exists(_ ~~ location)

  def updated(unitPosition: UnitPosition): UnitLocation = copy(locationUnitMap.updated(unitPosition.location, unitPosition))

  def clear(location: Location): UnitLocation = copy(locationUnitMap = locationUnitMap.filterNot { case (l, _) => l ~~ location })

  def units: List[UnitPosition] = locationUnitMap.values.toList

  def filterOrders(orders: Seq[Order], worldMap: WorldMap): Seq[Order] = {
    orders.collect {
      case (o: NonMoveOrder) if locationUnitMap.exists { case (_, up) => o.unitPosition ~~ up } => o
      case (o: MoveOrder) if locationUnitMap.exists { case (_, up) => o.unitPosition == up } => o
      case (o: MoveOrder) if locationUnitMap.exists { case (_, up) => o.unitPosition ~~ up } && o.unitType == Fleet =>
        locationUnitMap.find { case (_, up) => o.unitPosition ~~ up }.fold(o) {
          case (l, _) => o.copy(unitPosition = o.unitPosition.copy(location = l), dst = o.dst.setDstCoast(o.unitType, l, worldMap))
        } // 6.B.10
    }
  }
}

case class UnitPosition(power: Power, unitType: UnitType, location: Location) {
  def ~~(that: UnitPosition): Boolean = this.power == that.power && this.unitType == that.unitType && this.location ~~ that.location

  require(location.coast.isDefined)

  override def toString: String = s"$power: $unitType $location "

}

case class Power(name: String, active: Boolean, adjective: String) {
  override def toString: String = name
}

case class MapDefinition(id: String, title: String, URI: String,
                         thumbURI: String, preferredUnitStyle: String)

object Season {

  case object Spring extends Season

  case object Fall extends Season

}

trait Season


case class Turn(year: Int, season: Season) {
  def next: Turn = {
    season match {
      case Spring => copy(season = Fall)
      case Fall => copy(year = year + 1, season = Spring)
    }
  }
}


case class Phase(year: Int, season: Season, phaseType: PhaseType) {
  def turn: Turn = Turn(year, season)

  def next: Phase = {
    phaseType match {
      case Movement => copy(phaseType = Retreat)
      case Retreat => season match {
        case Spring => copy(season = Season.Fall, phaseType = Movement)
        case Fall => copy(phaseType = Adjustment)
      }
      case Adjustment => copy(year = year + 1, season = Spring, phaseType = Movement)
    }
  }
}

object Phase extends PhaseParser {
  def parse(input: String): Phase = parseAll(phase, input) match {
    case Success(data, next) => data
    case NoSuccess(errorMessage, next) => throw new RuntimeException(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }
}

trait PhaseParser extends RegexParsers {
  def phase: Parser[Phase] = season ~ (year <~ opt(",")) ~ (opt("(") ~> phasetype <~ opt(")")) ^^ { case (s ~ y ~ pt) => Phase(y, s, pt) }

  def season: Parser[Season] = (spring | fall) <~ opt(",")

  def spring: Parser[Spring.type] = "Spring" ^^ { _ => Spring }

  def fall: Parser[Fall.type] = "Fall" ^^ { _ => Fall }

  def year: Parser[Int] = """\d\d\d\d""".r ^^ { result => result.toInt }

  def phasetype: Parser[PhaseType] = movement | retreat | adjustment

  def movement: Parser[Movement.type] = "Movement" ^^ { _ => Movement }

  def retreat: Parser[Retreat.type] = "Retreat" ^^ { _ => Retreat }

  def adjustment: Parser[Adjustment.type] = "Adjustment" ^^ { _ => Adjustment }
}

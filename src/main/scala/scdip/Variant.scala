package scdip

import scdip.PhaseType.{Adjustment, Movement, Retreat}
import scdip.Season.{Fall, Spring}

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
    val powerMap: Map[String, Power] = (variantElem \ "POWER").map { elem =>
      (Power(elem \@ "name", (elem \@ "active").toBoolean, elem \@ "adjective"), Option(elem \@ "altnames").filter(_.nonEmpty))
    }.foldLeft(TreeMap.empty[String, Power](Ordering.by(_.toLowerCase))) {
      case (m, (p, alt)) => alt.foldLeft(m)(_.updated(_, p)).updated(p.name, p)
    }

    Variant(variantElem \@ "name",
      variantElem \ "MAP" \@ "adjacencyURI",
      (variantElem \ "RULEOPTION").foldLeft(Map.empty[String, String])((m, elem) =>
        m.updated(elem \@ "name", elem \@ "value")),
      powerMap,
      variantElem \ "STARTINGTIME" \@ "turn",
      (variantElem \ "VICTORYCONDITIONS").map(ve => VictoryCondition((ve \ "WINNING_SUPPLY_CENTERS" \@ "value").parseInt.toOption.getOrElse(0),
        (ve \ "YEARS_WITHOUT_SC_CAPTURE" \@ "value").parseInt.toOption.getOrElse(0),
        (ve \ "GAME_LENGTH" \@ "value").parseInt.toOption.getOrElse(0)
      )).headOption.getOrElse(VictoryCondition(0, 0, 0)),
      (variantElem \ "SUPPLYCENTER").map(n => (n \@ "province", n \@ "homepower", n \@ "owner")),
      (variantElem \ "INITIALSTATE").map(n => (n \@ "province", n \@ "power", n \@ "unit", n \@ "unitcoast")))
  })
  private val variantMap = variants.foldLeft(Map.empty[String, Variant])((m, v) => m.updated(v.name, v))

  def variant(name: String): Option[Variant] = variantMap.get(name)
}

case class InitialState()

case class Variant(name: String,
                   adjacencyURI: String, // TODO: to build WorldMap
                   ruleOptions: Map[String, String],
                   powerMap: Map[String, Power],
                   startingTime: String,
                   victoryCondition: VictoryCondition,
                   supplyCenterInfo: Seq[(String, String, String)],
                   initialState: Seq[(String, String, String, String)]
                  ) {

  def worldMap: WorldMap = WorldMap(XML.load(getClass.getResourceAsStream("/std_adjacency.xml"))) // TODO

  def power(name: String): Power = powerMap(name)

  def movementState: MovementState = {
    val home = supplyCenterInfo.foldLeft(Map.empty[Province, Option[Power]])((m, d) => m.updated(worldMap.province(d._1), if (d._2 == "none") {
      None
    } else {
      Option(power(d._2))
    }))
    val owner = supplyCenterInfo.foldLeft(Map.empty[Province, Option[Power]])((m, d) => m.updated(worldMap.province(d._1), if (d._3 == "none") {
      None
    } else {
      Option(power(d._3))
    }))
    val map = initialState.foldLeft(Map.empty[Location, GameUnit])((m, d) => {
      val unitType = UnitType.parse(d._3)
      val coast = if (d._4.isEmpty) {
        unitType.defaultCoast
      } else {
        Coast.parse(d._4)
      }
      val location = Location(worldMap.province(d._1), coast)
      m.updated(location, GameUnit(power(d._2), unitType))
    })

    MovementState(Phase.parse(startingTime).turn,
      SupplyCenterInfo(home, owner),
      UnitLocation(map),
      powerMap,
      worldMap,
      victoryCondition)
  }

}

case class VictoryCondition(winningSupplyCenters: Int, yearsWithoutScCapture: Int, gameLength: Int)

case class SupplyCenterInfo(home: Map[Province, Option[Power]], owner: Map[Province, Option[Power]])

case class UnitLocation(locationUnitMap: Map[Location, GameUnit]) {

  def updated(unitState: UnitState): UnitLocation = copy(locationUnitMap.updated(unitState.location, unitState.gameUnit))

  private val provinceMap = locationUnitMap.map { case (loc, unit) => (loc.province, (loc, unit)) }

  def getUnits(locations: Seq[Location]): Seq[UnitState] = {
    locations.flatMap(l => provinceMap.get(l.province)).map { case (loc, unit) => UnitState(loc, unit) }
  }

  def unitStats: List[UnitState] = {
    locationUnitMap.toList.map { case (l, g) => UnitState(l, g) }
  }
}

case class UnitState(location: Location, gameUnit: GameUnit)

case class Power(name: String, active: Boolean, adjective: String)

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
  def phase: Parser[Phase] = season ~ (year <~ opt(",")) ~ phasetype ^^ { case (s ~ y ~ pt) => Phase(y, s, pt) }

  def season: Parser[Season] = (spring | fall) <~ opt(",")

  def spring: Parser[Spring.type] = "Spring" ^^ { _ => Spring }

  def fall: Parser[Fall.type] = "Fall" ^^ { _ => Fall }

  def year: Parser[Int] = """\d\d\d\d""".r ^^ { result => result.toInt }

  def phasetype: Parser[PhaseType] = movement | retreat | adjustment

  def movement: Parser[Movement.type] = "Movement" ^^ { _ => Movement }

  def retreat: Parser[Retreat.type] = "Retreat" ^^ { _ => Retreat }

  def adjustment: Parser[Adjustment.type] = "Adjustment" ^^ { _ => Adjustment }
}
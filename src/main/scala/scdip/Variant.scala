package scdip

import scdip.PhaseType.{Adjustment, Movement, Retreat}
import scdip.Season.{Fall, Spring}

import scala.collection.immutable.Seq
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
    val powers = (variantElem \ "POWER").map(elem =>
      Power(elem \@ "name", (elem \@ "active").toBoolean, elem \@ "adjective"))
    val powerRef = powers.foldLeft(Map.empty[String, Power])((m, p) => m.updated(p.name, p))
    Variant(variantElem \@ "name",
      variantElem \ "MAP" \@ "adjacencyURI",
      (variantElem \ "RULEOPTION").foldLeft(Map.empty[String, String])((m, elem) =>
        m.updated(elem \@ "name", elem \@ "value")),
      powers,
      variantElem \ "STARTINGTIME" \@ "turn",
      (variantElem \ "VICTORYCONDITIONS").map(ve => VictoryCondition((ve \ "WINNING_SUPPLY_CENTERS" \@ "value").parseInt.toOption.getOrElse(0),
        (ve \ "YEARS_WITHOUT_SC_CAPTURE" \@ "value").parseInt.toOption.getOrElse(0),
        (ve \ "GAME_LENGTH" \@ "value").parseInt.toOption.getOrElse(0)
      )).headOption.getOrElse(VictoryCondition(0, 0, 0)),
      SupplyCenterInfo((variantElem \ "SUPPLYCENTER").map(n => (n \@ "province", n \@ "homepower", n \@ "owner"))),
      InitialState((variantElem \ "INITIALSTATE").map(n => (n \@ "province", n \@ "power", n \@ "unit", n \@ "unitcoast"))))
  })

  def variant(name: String): Option[Variant] = variants.find(_.name == name)
}

case class SupplyCenterInfo(data: Seq[(String, String, String)]) {
}

case class UnitPlacement() {

}

case class InitialState(data: Seq[(String, String, String, String)])

case class VictoryCondition(winningSupplyCenters: Int, yearsWithoutScCapture: Int, gameLength: Int)

case class Variant(name: String,
                   adjacencyURI: String,
                   ruleOptions: Map[String, String],
                   powers: Seq[Power],
                   startingTime: String, // FIXME
                   victoryCondition: VictoryCondition,
                   supplyCenterInfo: SupplyCenterInfo,
                   initialState: InitialState
                  ) {
  private val powerMap = powers.foldLeft(Map.empty[String, Power])((m, p) => m.updated(p.name, p))

  def worldMap: WorldMap = WorldMap(XML.load(getClass.getResourceAsStream("/std_adjacency.xml")))

  def power(name: String): Power = powerMap(name)

}

case class Power(name: String, active: Boolean, adjective: String)

case class MapDefinition(id: String, title: String, URI: String,
                         thumbURI: String, preferredUnitStyle: String)

object Season {

  case object Spring extends Season

  case object Fall extends Season

}

trait Season

object PhaseType {

  case object Movement extends PhaseType

  case object Retreat extends PhaseType

  case object Adjustment extends PhaseType

}

trait PhaseType

case class Phase(year: Int, season: Season, phaseType: PhaseType) {
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

case class UnitState(power:Power, unitType: UnitType, location: Location)
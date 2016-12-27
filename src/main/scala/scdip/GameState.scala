package scdip

import scdip.Order.MoveOrder

trait GameState {
  def phaseType: PhaseType

  def next(orderResults: Seq[OrderResult]): GameState
}

case class MovementState(turn: Turn,
                         supplyCenterInfo: SupplyCenterInfo,
                         unitLocation: UnitLocation,
                         powers: Map[String, Power], worldMap: WorldMap, victoryCondition: VictoryCondition) extends GameState {
  val phaseType = PhaseType.Movement

  override def next(orderResults: Seq[OrderResult]): GameState = {
    val targets: Set[Location] = orderResults.flatMap {
      case (or) => or.flatRun {
        case a: MoveOrder => Option(a.dst)
        case _ => None
      }
    }.toSet
    val origins: Set[Location] = orderResults.flatMap {
      case (or) => or.flatRun {
        case a: MoveOrder => Option(a.src)
        case _ => None
      }
    }.toSet
    val dislodgedLocations = targets -- origins
    val dislodgedUnits = unitLocation.getUnits(dislodgedLocations.toSeq)
    val clearedUnitLocation = origins.foldLeft(unitLocation) { (u, or) => u.clear(or) }
    val newUnitLocation = orderResults.foldLeft(clearedUnitLocation) {
      case (u, or) => or.run {
        case (a: MoveOrder) => u.updated(UnitState(a.dst, or.gameUnit))
        case _ => u
      }.getOrElse(u)
    }
    RetreatState(turn, supplyCenterInfo, newUnitLocation, dislodgedUnits)
  }
}

case class RetreatState(turn: Turn, supplyCenterInfo: SupplyCenterInfo, unitLocation: UnitLocation, dislodgeUnits: Seq[UnitState]) extends GameState {
  override val phaseType = PhaseType.Retreat

  override def next(orderResults: Seq[OrderResult]): GameState = ???
}

case class AdjustmentState() extends GameState {
  override val phaseType = PhaseType.Adjustment

  override def next(orderResults: Seq[OrderResult]): GameState = ???
}

object PhaseType {

  case object Movement extends PhaseType

  case object Retreat extends PhaseType

  case object Adjustment extends PhaseType

}

trait PhaseType

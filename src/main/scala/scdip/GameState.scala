package scdip

import scdip.Order.MoveOrder

trait GameState {
  def phaseType: PhaseType

  def next(orders: Seq[Order]): GameState
}

case class MovementState(turn: Turn,
                         supplyCenterInfo: SupplyCenterInfo,
                         unitLocation: UnitLocation,
                         powers: Map[String, Power], worldMap: WorldMap, victoryCondition: VictoryCondition) extends GameState {
  val phaseType = PhaseType.Movement

  override def next(orders: Seq[Order]): GameState = {
    val orderResults = OrderState(unitLocation.filterOrders(orders, worldMap), worldMap).resolve

    val moves: Seq[MoveOrder] = orderResults.results.flatMap {
      case (or) => or.flatRun {
        case a: MoveOrder => Option(a)
        case _ => None
      }
    }
    val newUnitLocation = move(orderResults.dislodgedList.provinces.foldLeft(unitLocation) { (u, l) => u.clear(l) }, moves)
    // TODO: disband
    RetreatState(turn, supplyCenterInfo, newUnitLocation, unitLocation.getUnits(orderResults.retreatLocations(worldMap)))
  }

  @scala.annotation.tailrec
  private def move(unitLocation: UnitLocation, moves: Seq[MoveOrder]): UnitLocation = {
    if (moves.isEmpty) {
      unitLocation
    } else {
      val (s, f) = moves.partition(m => unitLocation.isEmpty(m.dst))
      if (s.isEmpty && f.nonEmpty) {
        // maybe circular
        f.foldLeft(f.foldLeft(unitLocation) {
          case (u, m) => u.clear(m.src)
        }) {
          case (u, m) => u.updated(UnitState(m.dst, m.gameUnit))
        }
      } else {
        move(s.foldLeft(unitLocation) {
          case (u, m) => u.clear(m.src).updated(UnitState(m.dst, m.gameUnit))
        }, f)
      }
    }
  }
}

// TODO: dislodged and disrupted
case class RetreatState(turn: Turn, supplyCenterInfo: SupplyCenterInfo, unitLocation: UnitLocation, dislodgeUnits: Seq[UnitState]) extends GameState {
  override val phaseType = PhaseType.Retreat

  override def next(orders: Seq[Order]): GameState = ???
}

case class AdjustmentState() extends GameState {
  override val phaseType = PhaseType.Adjustment

  override def next(orders: Seq[Order]): GameState = ???
}

object PhaseType {

  case object Movement extends PhaseType

  case object Retreat extends PhaseType

  case object Adjustment extends PhaseType

}

trait PhaseType

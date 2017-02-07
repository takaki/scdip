package scdip

import scdip.Order.MoveOrder
import scdip.Season.{Fall, Spring}

trait GameState {
  def phaseType: PhaseType
  def unitLocation: UnitLocation
  def next(orders: Seq[Order]): GameState
}

case class WorldInfo(worldMap: WorldMap,  victoryCondition: VictoryCondition)

case class MovementState(worldInfo: WorldInfo,
                         turn: Turn,
                         unitLocation: UnitLocation,
                         supplyCenterInfo: SupplyCenterInfo
                        ) extends GameState {
  val phaseType = PhaseType.Movement

  override def next(orders: Seq[Order]): RetreatState = {
    val orderResults = OrderState(unitLocation.filterOrders(orders, worldInfo.worldMap), worldInfo.worldMap).resolve
    RetreatState(worldInfo, turn, orderResults.doMove(unitLocation), supplyCenterInfo, orderResults)
  }

}

// TODO: dislodged and disrupted
case class RetreatState(worldInfo: WorldInfo,
                        turn: Turn,
                        unitLocation: UnitLocation,
                        supplyCenterInfo: SupplyCenterInfo,
                        orderResults: OrderResults) extends GameState {

  override val phaseType = PhaseType.Retreat

  val dislodgeUnits: Seq[UnitPosition] = orderResults.retreatUnitPositions(worldInfo.worldMap)

  val disbandUnits: Seq[UnitPosition] = orderResults.disbandUnitPosittions(worldInfo.worldMap)

  override def next(orders: Seq[Order]): GameState = {
    val newUL = orders.collect {
      case (o: MoveOrder) => o
    }.filter(m => orderResults.retreatArea(worldInfo.worldMap, m.unitPosition).contains(m.dst)).foldLeft(unitLocation) {
      case (ul, m) => ul.clear(m.src).updated(UnitPosition(m.dst, m.gameUnit))
    }
    turn.season match {
      case Spring => MovementState(worldInfo, turn.next, newUL, supplyCenterInfo)
      case Fall => AdjustmentState(worldInfo, turn.next, newUL, supplyCenterInfo)
    }
  }
}

case class AdjustmentState(worldInfo: WorldInfo,
                           turn: Turn,
                           unitLocation: UnitLocation,
                           supplyCenterInfo: SupplyCenterInfo) extends GameState {
  override val phaseType = PhaseType.Adjustment

  def next(orders: Seq[Order]): MovementState = ???
}

object PhaseType {

  case object Movement extends PhaseType

  case object Retreat extends PhaseType

  case object Adjustment extends PhaseType

}

trait PhaseType

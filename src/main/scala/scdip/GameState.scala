package scdip

trait GameState {
  def phaseType: PhaseType

  def next(orders: Seq[Order]): GameState
}

case class MovementState(worldMap: WorldMap,
                         turn: Turn,
                         unitLocation: UnitLocation,
                         supplyCenterInfo: SupplyCenterInfo,
                         powers: Map[String, Power],
                         victoryCondition: VictoryCondition) extends GameState {
  val phaseType = PhaseType.Movement

  override def next(orders: Seq[Order]): RetreatState = {
    val orderResults = OrderState(unitLocation.filterOrders(orders, worldMap), worldMap).resolve
    RetreatState(worldMap, turn, orderResults.doMove(unitLocation), supplyCenterInfo, orderResults)
  }

}

// TODO: dislodged and disrupted
case class RetreatState(worldMap: WorldMap, turn: Turn, unitLocation: UnitLocation, supplyCenterInfo: SupplyCenterInfo, orderResults: OrderResults) extends GameState {

  override val phaseType = PhaseType.Retreat

  val dislodgeUnits: Seq[UnitPosition] = orderResults.retreatUnitPositions(worldMap)

  val disbandUnits: Seq[UnitPosition] = orderResults.disbandUnitPosittions(worldMap)

  override def next(orders: Seq[Order]): GameState = ???
}

case class AdjustmentState(turn: Turn,
                           supplyCenterInfo: SupplyCenterInfo,
                           unitLocation: UnitLocation) extends GameState {
  override val phaseType = PhaseType.Adjustment

  override def next(orders: Seq[Order]): GameState = ???
}

object PhaseType {

  case object Movement extends PhaseType

  case object Retreat extends PhaseType

  case object Adjustment extends PhaseType

}

trait PhaseType

package scdip

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
    val targets: Set[Location] = orderResults.flatMap(or => or.run(a => a.moveTarget)).flatten.toSet
    val origins: Set[Location] = orderResults.flatMap(or => or.run(a => a.moveOrigin)).flatten.toSet
    val dislodgedLocations = targets.diff(origins)
    val dislodgedUnits = unitLocation.getUnits(dislodgedLocations.toSeq)
    val newUnitLocation = orderResults.foldLeft(unitLocation)((u, or) => or.run(a => a.moveTarget).flatten.fold(u)(l => u.updated(UnitState(l, or.gameUnit))))
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

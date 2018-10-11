package scdip

import scdip.Order.{BuildOrder, MoveOrder, RemoveOrder}
import scdip.Season.{Fall, Spring}
import scdip.UnitType.{Army, Fleet}

trait GameState {
  def worldInfo: WorldInfo

  def worldMap: WorldMap = worldInfo.worldMap

  def unitLocation: UnitLocation

  def next(orders: Seq[Order]): GameState
}

case class WorldInfo(worldMap: WorldMap, powers: Seq[Power], victoryCondition: VictoryCondition)

case class MovementState(worldInfo: WorldInfo,
                         turn: Turn,
                         unitLocation: UnitLocation,
                         supplyCenterInfo: SupplyCenterInfo
                        ) extends GameState {
  override def next(orders: Seq[Order]): RetreatState = {
    val orderResults = OrderState(unitLocation.filterOrders(orders, worldMap), worldMap).resolve
    RetreatState(worldInfo, turn, orderResults.doMove(unitLocation), supplyCenterInfo, orderResults)
  }

}

case class RetreatState(worldInfo: WorldInfo,
                        turn: Turn,
                        unitLocation: UnitLocation,
                        supplyCenterInfo: SupplyCenterInfo,
                        orderResults: OrderResults) extends GameState {

  val dislodgeUnits: Seq[UnitPosition] = orderResults.retreatUnitPositions(worldMap)

  val disbandUnits: Seq[UnitPosition] = orderResults.disbandUnitPosittions(worldMap)

  override def next(orders: Seq[Order]): GameState = {
    val conflicts: Set[Province] = orders.collect {
      case m: MoveOrder if dislodgeUnits.contains(m.unitPosition) &&
        orderResults.retreatArea(worldMap, m.unitPosition).contains(m.dst) => m.dst.province
    }.groupBy(identity).collect { case (x, List(_, _, _*)) => x }.toSet

    val newUL = orders.foldLeft(unitLocation) {
      case (ul, m: MoveOrder) if dislodgeUnits.contains(m.unitPosition) &&
        (orderResults.retreatArea(worldMap, m.unitPosition).contains(m.dst) &&
          !conflicts.contains(m.dst.province)) => ul.updated(UnitPosition(m.power, m.unitType, m.dst))
      case (ul, _) => ul
    }

    turn.season match {
      case Spring => MovementState(worldInfo, turn.next, newUL, supplyCenterInfo)
      case Fall => AdjustmentState(worldInfo, turn, newUL, supplyCenterInfo)
    }
  }

}

case class AdjustmentState(worldInfo: WorldInfo,
                           turn: Turn,
                           unitLocation: UnitLocation,
                           supplyCenterInfo: SupplyCenterInfo) extends GameState {

  def next(orders: Seq[Order]): MovementState = {
    @scala.annotation.tailrec
    def civilDisorder(power: Power, unitLocation: UnitLocation, supplyCenterInfo: SupplyCenterInfo): UnitLocation = {
      if (unitLocation.count(power) <= supplyCenterInfo.count(power)) {
        unitLocation
      } else {
        val rm = unitLocation.ownUnits(power).minBy { up =>
          (up.unitType match {
            case Fleet => 0
            case Army => 1
          }, -supplyCenterInfo.ownHomes(power).map(p => worldMap.distance(Location(p, Option(Coast.Land)), up.location)).min)
        }
        civilDisorder(power, unitLocation.clear(rm.location), supplyCenterInfo)
      }
    }

    val newSCI = unitLocation.units.foldLeft(supplyCenterInfo) {
      case (sc, up) => sc.updated(up.location.province, up.power)
    }

    val newUL = orders.foldLeft(unitLocation) {
      case (ul, o: BuildOrder) if ul.count(o.power) < newSCI.count(o.power) &&
        newSCI.isHome(o.src.province, o.power) &&
        ul.isClear(o.src) && worldMap.exists(o.src) &&
        newSCI.isOwner(o.src.province, o.power)
      => ul.updated(o.unitPosition)
      case (ul, o: RemoveOrder) if ul.count(o.power) > newSCI.count(o.power) &&
        ul.exists(o.unitPosition)
      => ul.clear(o.src)
      case (ul, _) => ul
    }

    MovementState(worldInfo, turn.next, worldInfo.powers.foldLeft(newUL) { case (ul, p) => civilDisorder(p, ul, newSCI) }, newSCI)
  }
}


package scdip

import scdip.Action._
import scdip.Order._

object UnitType {

  object Fleet extends UnitType("Fleet") {
    override def defaultCoast: Coast = Coast.Single
  }

  object Army extends UnitType("Army") {
    override def defaultCoast: Coast = Coast.Land
  }

  def parse(s: String): UnitType = {
    s match {
      case "Fleet" => Fleet
      case "Army " => Army
      case _ => throw new IllegalArgumentException()
    }
  }
}

abstract sealed case class UnitType(name: String) {
  def defaultCoast: Coast
}


object Order {

  case class HoldOrder(power: Power, action: HoldAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class MoveOrder(power: Power, action: MoveAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class SupportHoldOrder(power: Power, action: SupportHoldAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class SupportMoveOrder(power: Power, action: SupportMoveAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class ConvoyOrder(power: Power, action: ConvoyAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

}

sealed trait Order {
  def power: Power

  def action: Action

  def mark: Option[String]

  def supportCount: Int

  def src: Location = action.src
}

object Action {

  case class HoldAction(unitType: UnitType, src: Location) extends Action

  case class MoveAction(unitType: UnitType, src: Location, dst: Location) extends Action

  case class SupportHoldAction(unitType: UnitType, src: Location, supportHold: HoldAction) extends Action

  case class SupportMoveAction(unitType: UnitType, src: Location, supportMove: MoveAction) extends Action

  case class ConvoyAction(unitType: UnitType, src: Location, convoyMove: MoveAction) extends Action

}

sealed trait Action {
  def unitType: UnitType

  def src: Location

}

trait OrderFilter {
  def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState
}

case object OrderEvaluateStep1 extends OrderFilter {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    val moves = orderState.moves.map(move =>
      if (move.action.unitType == UnitType.Army &&
        !worldMap.isNeighbour(move.src, move.action.dst) &&
        !worldMap.canConvoy(move.src.province, move.action.dst.province,
          restrict = orderState.convoys.filter(c => c.action.convoyMove == move.action)
            .map(c => c.src.province))) {
        move.copy(mark = Option("no convoy"))
      } else {
        move
      })
    orderState.copy(moves = moves)
  }
}

case class OrderState(holds: Seq[HoldOrder] = Seq.empty,
                      moves: Seq[MoveOrder] = Seq.empty,
                      supportHolds: Seq[SupportHoldOrder] = Seq.empty,
                      supportMoves: Seq[SupportMoveOrder] = Seq.empty,
                      convoys: Seq[ConvoyOrder] = Seq.empty
                     ) {
  //  def step2(worldMap: WorldMap): OrderState = {
  //    val st = moves.map(m => !worldMap.isNeighbour(m.src, m.dst)).foldLeft(mark)((s, m) => s.updated(m, "void"))
  //    this.copy(mark = st)
  //  }
}
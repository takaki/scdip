package scdip

import scdip.Action._


object Order {

  case class HoldOrder(power: Power, action: HoldAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class MoveOrder(power: Power, action: MoveAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order {
    def requireConvoy(worldMap: WorldMap): Boolean = !worldMap.isNeighbour(action.src, action.dst)
  }

  case class SupportHoldOrder(power: Power, action: SupportHoldAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class SupportMoveOrder(power: Power, action: SupportMoveAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class ConvoyOrder(power: Power, action: ConvoyAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class BuildOrder(power: Power, action: BuildAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

  case class RemoveOrder(power: Power, action: RemoveAction, mark: Option[String] = Option.empty, supportCount: Int = 0) extends Order

}

sealed trait Order {
  def power: Power

  def action: Action

  def mark: Option[String]

  def supportCount: Int

  def src: Location = action.src

  def result: OrderResult = if (mark.isEmpty) {
    SuccessResult(power, action)
  } else {
    FailureResult(power, action)
  }
}

object MovementOrder

object RetreatOrder

object AdjustmentOrder

object Action {

  case class HoldAction(unitType: UnitType, src: Location) extends Action

  case class MoveAction(unitType: UnitType, src: Location, dst: Location) extends Action {
    def ~~(action: MoveAction): Boolean = (action.src ~~ src) && (action.dst ~~ dst)

    override def moveOrigin: Option[Location] = Option(src)

    override def moveTarget: Option[Location] = Option(dst)
  }

  case class SupportHoldAction(unitType: UnitType, src: Location, supportHold: HoldAction) extends Action

  case class SupportMoveAction(unitType: UnitType, src: Location, supportMove: MoveAction) extends Action

  case class ConvoyAction(unitType: UnitType, src: Location, convoyMove: MoveAction) extends Action

  case class BuildAction(unitType: UnitType, src: Location) extends Action

  case class RemoveAction(unitType: UnitType, src: Location) extends Action

}

sealed trait Action {
  def unitType: UnitType

  def src: Location

  def moveOrigin: Option[Location] = None

  def moveTarget: Option[Location] = None

}


object UnitType {

  def parse(input: String): UnitType = input match {
    case "(?i)fleet" => Fleet
    case "(?i)army" => Army
    case _ => throw new IllegalArgumentException()
  }

  object Fleet extends UnitType {
    override def toString: String = "F"

    override def defaultCoast: Coast = Coast.Single
  }

  object Army extends UnitType {
    override def toString: String = "A"

    override def defaultCoast: Coast = Coast.Land

  }

}

case class GameUnit(power: Power, unitType: UnitType) {
  override def toString: String = s"$power: $unitType"
}

sealed trait UnitType {
  def defaultCoast: Coast
}



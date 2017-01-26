package scdip


object Order {

  case class HoldOrder(power: Power, unitType: UnitType, src: Location) extends NonMoveOrder {
    override def toString: String = s"$power: $unitType $src H"
  }

  case class MoveOrder(power: Power, unitType: UnitType, src: Location, dst: Location, explictConvoy: Boolean = false) extends Order {


    override def toString: String = s"$power: $unitType $src - $dst${if (explictConvoy) " via Convoy" else ""}"

    def ~~(moveOrder: MoveOrder): Boolean = src ~~ moveOrder.src && dst ~~ moveOrder.dst

    def canConvoy(worldMap: WorldMap, convoys: Seq[ConvoyOrder]): Boolean = {
      worldMap.canConvoy(src.province, dst.province,
        convoys = convoys.map(_.src.province).toSet)
    }

    def isNeighbour(worldMap: WorldMap): Boolean = worldMap.isNeighbour(src, dst)
  }

  trait SupportOrder extends NonMoveOrder {

    def canSupport(o: Order): Boolean

    def findSupportTarget(orders: Seq[Order]): Option[Order]

    def reachSupport(worldMap: WorldMap): Boolean

  }

  case class SupportHoldOrder(power: Power, unitType: UnitType, src: Location, targetUnit: UnitType, targetSrc: Location) extends SupportOrder {
    override def toString: String = s"$power: $unitType $src S $targetUnit $targetSrc"

    override def canSupport(o: Order): Boolean = o match {
      case nm: NonMoveOrder => targetSrc ~~ nm.src
      case _ => false
    }

    override def findSupportTarget(orders: Seq[Order]): Option[Order] = {
      orders.find(order => targetSrc ~~ order.src)
    }

    override def reachSupport(worldMap: WorldMap): Boolean = worldMap.isReachable(src, targetSrc)

  }

  case class SupportMoveOrder(power: Power, unitType: UnitType, src: Location, targetUnit: UnitType, from: Location, to: Location) extends SupportOrder {
    override def toString: String = s"$power: $unitType $src S $targetUnit $from - $to"

    override def canSupport(o: Order): Boolean = o match {
      case m: MoveOrder => from ~~ m.src && ((to.coast.isEmpty && to ~~ m.dst) || (to.coast.isDefined && to == m.dst))
      case _ => false
    }

    override def findSupportTarget(orders: Seq[Order]): Option[Order] = {
      orders.find {
        case m: MoveOrder => canSupport(m)
        case _ => false
      }
    }

    override def reachSupport(worldMap: WorldMap): Boolean = worldMap.isReachable(src, to)

  }

  case class ConvoyOrder(power: Power, unitType: UnitType, src: Location, targetUnit: UnitType, from: Location, to: Location) extends NonMoveOrder {
    override def toString: String = s"$power: $unitType $src C $targetUnit $from - $to"

    def findConvoyTarget(moves: Seq[MoveOrder]): Option[MoveOrder] = {
      moves.find(m => from ~~ m.src && to ~~ m.dst)
    }

  }

  case class BuildOrder(power: Power, unitType: UnitType, src: Location) extends Order

  case class RemoveOrder(power: Power, unitType: UnitType, src: Location) extends Order

}


sealed trait OrderBase {
  def power: Power

  def unitType: UnitType

  def src: Location

  def success = SuccessResult(this)

  def failure(orderMark: OrderMark) = FailureResult(this, Option(orderMark))

  def gameUnit: GameUnit = GameUnit(power, unitType)

}

sealed trait Order extends OrderBase

sealed trait NonMoveOrder extends Order

sealed trait RetreatOrder extends OrderBase

sealed trait AdjustmentOrder extends OrderBase

object MovementOrder

object RetreatOrder

object AdjustmentOrder

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



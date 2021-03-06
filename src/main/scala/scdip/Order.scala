package scdip

sealed trait Order {
  def unitPosition: UnitPosition

  def power: Power = unitPosition.power

  def unitType: UnitType = unitPosition.unitType

  def src: Location = unitPosition.location

  def success = SuccessResult(this)

  def failure(orderMark: OrderMark) = FailureResult(this, Option(orderMark))

}

sealed trait MovementOrder extends Order

sealed trait NonMoveOrder extends MovementOrder

sealed trait RetreatOrder extends Order

sealed trait AdjustmentOrder extends Order


object Order {

  case class HoldOrder(unitPosition: UnitPosition) extends NonMoveOrder {
    override def toString: String = s"$power: $unitType $src H"
  }

  case class MoveOrder(unitPosition: UnitPosition, dst: Location, explictConvoy: Boolean = false) extends Order {


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

  case class SupportHoldOrder(unitPosition: UnitPosition, targetUnit: UnitType, targetSrc: Location) extends SupportOrder {
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

  case class SupportMoveOrder(unitPosition: UnitPosition, targetUnit: UnitType, from: Location, to: Location) extends SupportOrder {
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

  case class ConvoyOrder(unitPosition: UnitPosition, targetUnit: UnitType, from: Location, to: Location) extends NonMoveOrder {
    override def toString: String = s"$power: $unitType $src C $targetUnit $from - $to"

    def findConvoyTarget(moves: Seq[MoveOrder]): Option[MoveOrder] = {
      moves.find(m => from ~~ m.src && to ~~ m.dst)
    }

  }


  case class RetreatMoveOrder(unitPosition: UnitPosition) extends RetreatOrder {}

  case class DestroyOrder(unitPosition: UnitPosition) extends RetreatOrder {}

  case class BuildOrder(unitPosition: UnitPosition) extends AdjustmentOrder

  case class RemoveOrder(unitPosition: UnitPosition) extends AdjustmentOrder

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

sealed trait UnitType {
  def defaultCoast: Coast
}



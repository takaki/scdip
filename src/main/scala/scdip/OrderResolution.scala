package scdip

import scdip.Order._
import scdip.OrderMark.{NoConvoy, VoidMark}
import scdip.UnitType.{Army, Fleet}


trait OrderAdjudicator {
  def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState
}

case object AdjudicatorStep1 extends OrderAdjudicator {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    val orders = orderState.orders
    val newOrders = orders.map {
      case move: MoveOrder =>
        move.action.unitType match {
          case Army =>
            if (move.requireConvoy(worldMap) && !move.canConvoy(worldMap, orders)) {
              move.copy(mark = Option(NoConvoy()))
            } else {
              move
            }
          case Fleet => move
        }
      case convoy: ConvoyOrder =>
        convoy.action.unitType match {
          case Army => convoy.copy(mark = Option(VoidMark()))
          case Fleet => if (convoy.findConvoyed(orders)) {
            convoy
          } else {
            convoy.copy(mark = Option(VoidMark()))
          }
        }
      case x => x
    }
    orderState.copy(orders = newOrders)
  }
}

case object AdjudicatorStep2 extends OrderAdjudicator {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    val orders = orderState.orders
    val newOrders = orders.map {
      case move: MoveOrder =>
        move.action.unitType match {
          case Army => if (worldMap.canConvoy(move.src.province, move.action.dst.province) ||
            worldMap.isNeighbour(move.action.src, move.action.dst)) {
            move
          } else {
            move.copy(mark = Option(VoidMark()))
          }
          case Fleet =>
            if (worldMap.isNeighbour(move.action.src, move.action.dst)) {
              move
            } else {
              move.copy(mark = Option(VoidMark()))
            }
        }
      case s: SupportHoldOrder =>
        if (s.canSupport(orders)) {
          if (s.reachSupport(worldMap)) {
            s
          } else {
            s.copy(mark = Option(VoidMark("fail reachSupport")))
          }
        } else {
          s.copy(mark = Option(VoidMark("fail canSupport")))
        }
      case s: SupportMoveOrder =>
        if (s.canSupport(orders)) {
          if (s.reachSupport(worldMap)) {
            s
          } else {
            s.copy(mark = Option(VoidMark("fail reachSupport")))
          }
        }
        else {
          s.copy(mark = Option(VoidMark("fail canSupport")))
        }
      case x => x
    }
    println(newOrders)
    val supportCount = newOrders.filter {
      case x: SupportHoldOrder => x.mark.isEmpty
      case x: SupportMoveOrder => x.mark.isEmpty
      case _ => false
    }.foldLeft(orderState.supportCount) { case (m, o) =>
      o match {
        case s: SupportHoldOrder =>
          val target = s.target.province
          m.updated(target, m.getOrElse(target, 0) + 1)
        case s: SupportMoveOrder =>
          val target = s.target.province
          m.updated(target, m.getOrElse(target, 0) + 1)
        case _ => m
      }
    }
    orderState.copy(orders = newOrders, supportCount = supportCount)
  }

}

case class OrderState(orders: Seq[Order], supportCount: Map[Province, Int] = Map.empty) {

  def results: Seq[OrderResult] = {
    orders.map(_.result)
  }

}


trait OrderResult {
  def power: Power

  def action: Action

  def gameUnit: GameUnit = GameUnit(power, action.unitType)

  def run[T](f: Action => T): Option[T]
}

case class SuccessResult(power: Power, action: Action) extends OrderResult {
  override def run[T](f: (Action) => T): Option[T] = Option(f(action))
}

case class FailureResult(power: Power, action: Action) extends OrderResult {
  override def run[T](f: (Action) => T): Option[T] = None
}


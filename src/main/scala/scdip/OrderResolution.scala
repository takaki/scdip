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
    orders.foldLeft(orderState) {
      case (os, move: MoveOrder) if move.isNeighbour(worldMap) => os
      case (os, move: MoveOrder) => move.action.unitType match {
        case Army if move.canConvoy(worldMap, orders) => os
        case Army => os.setMark(move, NoConvoy("no convoy path"))
        case Fleet => os.setMark(move, VoidMark("fleet can't jump"))
      }
      case (os, convoy: ConvoyOrder) => convoy.action.unitType match {
        case Army => os.setMark(convoy, VoidMark("army can't convoy"))
        case Fleet if convoy.findConvoyed(orders) => os
        case Fleet => os.setMark(convoy, VoidMark("no convoy target"))
      }
      case (os, _) => os
    }
  }

}

case object AdjudicatorStep2 extends OrderAdjudicator {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    val orders = orderState.orders
    val newOrderState = orders.foldLeft(orderState) {
      case (os, s: SupportOrder) if !s.existsSupportTarget(orders) => os.setMark(s, VoidMark("fail existsSupportTarget"))
      case (os, s: SupportOrder) if !s.reachSupport(worldMap) => os.setMark(s, VoidMark("fail reachSupport"))
      case (os, _) => os
    }
    newOrderState.orders.filter {
      case x: SupportOrder => newOrderState.getMark(x).isEmpty
      case _ => false
    }.foldLeft(newOrderState) {
      case (os, s: SupportHoldOrder) => os.addSupportCount(s)
      case (os, s: SupportMoveOrder) =>
        orders.foldLeft(os.addSupportCount(s)) {
          case (oss, m: MoveOrder) if m.action ~~ s.targetAction && orders.exists(o => o.src == m.action.dst && o.power == m.power) => {
            oss.addNoHelpList(m, s)
          }
          case (oss, _) => oss
        }
      case (os, _) => os
    }
  }

}

case class OrderState(orders: Seq[Order],
                      orderMark: Map[Order, OrderMark] = Map.empty,
                      supportCount: Map[Action, Int] = Map.empty,
                      noHelpList: Map[MoveOrder, Seq[SupportMoveOrder]] = Map.empty
                     ) {

  def results: Seq[OrderResult] = {
    orders.map(o => if (orderMark.get(o).isEmpty) o.success else o.failure)
  }

  def setMark(order: Order, mark: OrderMark): OrderState = {
    copy(orderMark = orderMark.updated(order, mark))
  }

  def getMark(order: Order): Option[OrderMark] = orderMark.get(order)

  def addSupportCount(supportOrder: SupportOrder): OrderState = {
    copy(supportCount = supportCount.updated(supportOrder.targetAction, supportCount.getOrElse(supportOrder.targetAction, 0) + 1))
  }

  def getSupprtCount(order: Order): Int = {
    supportCount.getOrElse(order.action, 0)
  }

  def addNoHelpList(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): OrderState = {
    copy(noHelpList = noHelpList.updated(moveOrder, noHelpList.getOrElse(moveOrder, Seq()) :+ supportMoveOrder))
  }

  def getNoHelpList(moveOrder: MoveOrder): Seq[SupportMoveOrder] = {
    noHelpList.get(moveOrder).toSeq.flatten
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


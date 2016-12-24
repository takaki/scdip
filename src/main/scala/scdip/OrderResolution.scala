package scdip

import scdip.Order._
import scdip.OrderMark.{CutMark, NoConvoy, VoidMark}
import scdip.UnitType.{Army, Fleet}

case object OrderResolution {
  def exec(orderState: OrderState, worldMap: WorldMap): OrderState = {
    Seq(AdjudicatorStep1,
      AdjudicatorStep2,
      AdjudicatorStep3).foldLeft(orderState)((os, oes) => oes.evaluate(worldMap, os))
  }
}


trait OrderAdjudicator {
  def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState
}

case object AdjudicatorStep1 extends OrderAdjudicator {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    val orders = orderState.orders
    orders.foldLeft(orderState) {
      case (os, move: MoveOrder) if move.isNeighbour(worldMap) => os
      case (os, move: MoveOrder) => move.action.unitType match {
        case Army if move.canConvoy(worldMap, orders) => os.copy(convoyingArmiesList = os.convoyingArmiesList + move)
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
    orders.foldLeft(orderState) {
      case (os, s: SupportOrder) if !s.existsSupportTarget(orders) => os.setMark(s, VoidMark("fail existsSupportTarget"))
      case (os, s: SupportOrder) if !s.reachSupport(worldMap) => os.setMark(s, VoidMark("fail reachSupport"))
      case (os, s: SupportHoldOrder) => os.incSupportCount(s)
      case (os, s: SupportMoveOrder) =>
        orders.foldLeft(os.incSupportCount(s)) {
          case (oss, m: MoveOrder) if m.action ~~ s.targetAction &&
            orders.exists(o => o.src ~~ m.action.dst && o.power == m.power) => oss.addNoHelpList(m, s)
          case (oss, _) => oss
        }
      case (os, _) => os
    }
  }

}

case object AdjudicatorStep3 extends OrderAdjudicator {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    // non-convoyed cut support
    val orders = orderState.orders
    val newOS = orderState.orderMatrix.foldLeft(orderState) {
      case (os, (m: MoveOrder, so: SupportHoldOrder)) if m.isNeighbour(worldMap) &&
        so.src ~~ m.action.dst &&
        os.getMark(so).isEmpty &&
        so.power != m.power => os.setMark(so, CutMark()).decSupportCount(so)
      case (os, (m: MoveOrder, so: SupportMoveOrder)) if m.isNeighbour(worldMap) &&
        so.src ~~ m.action.dst &&
        os.getMark(so).isEmpty &&
        so.power != m.power => os.setMark(so, CutMark()).decSupportCount(so).delNoHelpList(m, so)
      case (os, _) => os
    }
    // TODO: check mark no need?
    newOS.copy(combats = orderState.orders.flatMap {
      case (m: MoveOrder) => Seq(m.action.src.province, m.action.dst.province)
      case x => Seq(x.action.src.province)
    }.toSet)
  }

}

case object AdjudicatorStep4 extends OrderAdjudicator {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    orderState
  }
}

case class OrderState(orders: Seq[Order],
                      orderMark: Map[Order, OrderMark] = Map.empty,
                      convoyingArmiesList: Set[MoveOrder] = Set.empty,
                      supportCount: Map[Action, Int] = Map.empty,
                      noHelps: Map[MoveOrder, Set[SupportMoveOrder]] = Map.empty,
                      combats: Set[Province] = Set.empty,
                      convoyArmies: Set[MoveOrder] = Set.empty,
                      convoySuccess: Set[MoveOrder] = Set.empty) {
  def orderMatrix: Seq[(Order, Order)] = {
    for {
      x <- orders
      y <- orders
    } yield (x, y)
  }

  def results: Seq[OrderResult] = {
    orders.map(o => if (orderMark.get(o).isEmpty) o.success else o.failure)
  }

  def setMark(order: Order, mark: OrderMark): OrderState = {
    copy(orderMark = orderMark.updated(order, mark))
  }

  def getMark(order: Order): Option[OrderMark] = orderMark.get(order)

  def incSupportCount(supportOrder: SupportOrder): OrderState = {
    copy(supportCount = supportCount.updated(supportOrder.targetAction, supportCount.getOrElse(supportOrder.targetAction, 0) + 1))
  }

  def decSupportCount(supportOrder: SupportOrder): OrderState = {
    copy(supportCount = supportCount.updated(supportOrder.targetAction, supportCount.getOrElse(supportOrder.targetAction, 0) - 1))
  }

  def getSupprtCount(order: Order): Int = {
    supportCount.getOrElse(order.action, 0)
  }

  def addNoHelpList(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): OrderState = {
    copy(noHelps = noHelps.updated(moveOrder, noHelps.getOrElse(moveOrder, Set()) + supportMoveOrder))
  }

  def delNoHelpList(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): OrderState = {
    copy(noHelps = noHelps.updated(moveOrder, noHelps.getOrElse(moveOrder, Set()) - supportMoveOrder))
  }

  def getNoHelpList(moveOrder: MoveOrder): Seq[SupportMoveOrder] = {
    noHelps.get(moveOrder).toSeq.flatten
  }
}


trait OrderResult {
  def power: Power

  def action: Action

  def gameUnit: GameUnit = GameUnit(power, action.unitType)

  def run[T](f: Action => T): Option[T]

  def flatRun[T](f: Action => Option[T]): Option[T]
}

case class SuccessResult(power: Power, action: Action) extends OrderResult {
  override def run[T](f: (Action) => T): Option[T] = Option(f(action))

  override def flatRun[T](f: (Action) => Option[T]): Option[T] = f(action)
}

case class FailureResult(power: Power, action: Action) extends OrderResult {
  override def run[T](f: (Action) => T): Option[T] = None

  override def flatRun[T](f: (Action) => Option[T]): Option[T] = None
}

object OrderMark {

  case class VoidMark(message: String = "") extends OrderMark

  case class NoConvoy(message: String = "") extends OrderMark

  case class CutMark(message: String = "") extends OrderMark

}

sealed trait OrderMark {
  def message: String
}


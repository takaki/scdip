package scdip

import scdip.Order._
import scdip.OrderMark.{ConvoyEndangered, CutMark, NoConvoy, VoidMark}
import scdip.UnitType.{Army, Fleet}

// ref: http://www.floc.net/dpjudge/?page=Algorithm

case class OrderResolution(worldMap: WorldMap) {
  def exec(orderState: OrderState): OrderState = {
    orderState.evaluate(step1)
      .evaluate(step2)
      .evaluate(step3)
  }

  // Step 1. Mark All Invalid Convoy Orders
  def step1(orderState: OrderState): OrderState = {
    val orders = orderState.orders
    orders.foldLeft(orderState) {
      case (os, move: MoveOrder) if move.isNeighbour(worldMap) => os
      case (os, move: MoveOrder) => move.action.unitType match {
        case Army => if (move.canConvoy(worldMap, orders)) os else os.setMark(move, NoConvoy("no convoy path"))
        case Fleet => os.setMark(move, VoidMark("fleet can't jump"))
      }
      case (os, convoy: ConvoyOrder) => convoy.action.unitType match {
        case Army => os.setMark(convoy, VoidMark("army can't convoy"))
        case Fleet => convoy.findConvoyed(orders).fold(os.setMark(convoy, VoidMark("no convoy target")))(m => os.copy(convoyingArmies = os.convoyingArmies + (convoy -> m)))
      }
      case (os, _) => os
    }
  }

  // Step 2. Mark All Invalid Move and Support Orders
  def step2(orderState: OrderState): OrderState = {
    val orders = orderState.orders
    orderState.orderMatrix.foldLeft(orderState) {
      case (os, (s: SupportOrder, _)) if !s.existsSupportTarget(orders) => os.setMark(s, VoidMark("fail existsSupportTarget"))
      case (os, (s: SupportOrder, _)) if !s.reachSupport(worldMap) => os.setMark(s, VoidMark("fail reachSupport"))
      case (os, (s: SupportHoldOrder, nm: NonMoveOrder)) if s.targetAction.src ~~ nm.src => os.incSupportCount(s, nm)
      case (os, (s: SupportMoveOrder, m: MoveOrder)) if s.targetAction ~~ m.action => (if (orders.exists(o => o.src ~~ m.action.dst && o.power == m.power)) {
        os.addNoHelpList(m, s)
      } else {
        os
      }).incSupportCount(s, m)
      case (os, _) => os
    }
  }

  //  Step 3. Calculate Initial Combat Strengths
  def step3(orderState: OrderState): OrderState = {
    // non-convoyed cut support
    val orders = orderState.orders
    val newOS = orderState.orderMatrix.foldLeft(orderState) {
      case (os, (m: MoveOrder, so: SupportHoldOrder)) if m.isNeighbour(worldMap) &&
        so.src ~~ m.action.dst &&
        os.getMark(so).isEmpty && //  cut or void
        so.power != m.power => os.setMark(so, CutMark()).decSupportCount(so)
      case (os, (m: MoveOrder, so: SupportMoveOrder)) if m.isNeighbour(worldMap) &&
        so.src ~~ m.action.dst &&
        os.getMark(so).isEmpty &&
        so.power != m.power => os.setMark(so, CutMark()).decSupportCount(so).delNoHelpList(m, so)
      case (os, _) => os
    }
    // TODO: check mark no need?
    newOS.copy(combats = orderState.orders.map {
      case (m: MoveOrder) => m.action.dst.province
      case x => x.action.src.province
    }.toSet)
  }


  // Step 4. Mark Support Cuts Made by Convoyers and Mark Endangered Convoys
  def step4(orderState: OrderState): OrderState = {
    // checkDisruption
    val newOS = orderState.fold {
      case (os, c: ConvoyOrder) if c.action.unitType == Fleet && os.combats.contains(c.action.src.province) =>
        os.uniqueHighestSupportedOrder(c.src.province).flatMap(ho => if (ho.power != c.power) {
          os.convoyingArmies.get(c).map(m => os.setMark(m, ConvoyEndangered()))
        } else {
          None
        }).getOrElse(os)
      case (os, _) => os
    }
    val newOS2 = newOS.copy(convoySuccess = newOS.convoyingArmies.values.filter(o => newOS.getMark(o).isEmpty).toSet)
    newOS2
  }

  // Step 5. Mark Convoy Disruptions And Support Cuts Made by Successful Convoys
  // Step 6. Mark Bounces Caused by Inability to Swap Places
  // Step 7. Mark Bounces Suffered by Understrength Attackers
  // Step 8. Mark Bounces Caused by Inability to Self-Dislodge
  // Step 9. Mark Supports Cut By Dislodgements

}

case class OrderState(orders: Seq[Order],
                      orderMark: Map[Order, OrderMark] = Map.empty,
                      supportCount: SupportCount = SupportCount(),
                      noHelps: Map[MoveOrder, Set[SupportMoveOrder]] = Map.empty,
                      combats: Set[Province] = Set.empty,
                      convoyingArmies: Map[ConvoyOrder, MoveOrder] = Map.empty,
                      convoySuccess: Set[MoveOrder] = Set.empty) {

  def evaluate(evaluator: OrderState => OrderState): OrderState = {
    evaluator(this)
  }


  def orderMatrix: Seq[(Order, Order)] = {
    for {
      x <- orders
      y <- orders
    } yield (x, y)
  }

  def fold(f: (OrderState, Order) => OrderState): OrderState = {
    orders.foldLeft(this)(f)
  }

  def results: Seq[OrderResult] = {
    orders.map(o => if (orderMark.get(o).isEmpty) o.success else o.failure)
  }

  def setMark(order: Order, mark: OrderMark): OrderState = {
    copy(orderMark = orderMark.updated(order, mark))
  }

  def getMark(order: Order): Option[OrderMark] = orderMark.get(order)

  def incSupportCount(supportHoldOrder: SupportHoldOrder, nonMoveOrder: NonMoveOrder): OrderState = {
    copy(supportCount = supportCount.incSupport(supportHoldOrder, nonMoveOrder))
  }

  def incSupportCount(supportMoveOrder: SupportMoveOrder, moveOrder: MoveOrder): OrderState = {
    copy(supportCount = supportCount.incSupport(supportMoveOrder, moveOrder))
  }

  def decSupportCount(supportOrder: SupportOrder): OrderState = {
    copy(supportCount = supportCount.decSupport(supportOrder))
  }

  def getSupportCount(order: Order): Int = {
    supportCount.strength(order)
  }

  def uniqueHighestSupportedOrder(province: Province): Option[Order] = {
    orders.filter {
      case (m: MoveOrder) => m.action.dst ~~ province
      case (o) => o.src ~~ province
    }.map(o => o -> supportCount.strength(o)).groupBy { case (o, s) => s }.toSeq.sortBy {
      case (s, o) => s
    }.reverse.headOption.flatMap {
      case (o, s) => if (s.size == 1) Option(s.head._1) else None
    }
  }


  def addNoHelpList(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): OrderState
  = {
    copy(noHelps = noHelps.updated(moveOrder, noHelps.getOrElse(moveOrder, Set()) + supportMoveOrder))
  }

  def delNoHelpList(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): OrderState = {
    copy(noHelps = noHelps.updated(moveOrder, noHelps.getOrElse(moveOrder, Set()) - supportMoveOrder))
  }

  def getNoHelpList(moveOrder: MoveOrder): Seq[SupportMoveOrder] = {
    noHelps.get(moveOrder).toSeq.flatten
  }
}

case class SupportCount(map: Map[SupportOrder, Order] = Map.empty) {
  def incSupport(supportMoveOrder: SupportMoveOrder, moveOrder: MoveOrder): SupportCount = {
    copy(map = map.updated(supportMoveOrder, moveOrder))
  }

  def incSupport(supportHoldOrder: SupportHoldOrder, nonMoveOrder: NonMoveOrder): SupportCount = {
    copy(map = map.updated(supportHoldOrder, nonMoveOrder))
  }

  def decSupport(supportOrder: SupportOrder): SupportCount = {
    copy(map = map - supportOrder)
  }

  def strength(order: Order): Int = {
    map.count { case (k, v) => v == order }
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

  case class ConvoyEndangered(message: String = "") extends OrderMark

}


sealed trait OrderMark {
  def message: String
}


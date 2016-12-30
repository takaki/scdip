package scdip

import scdip.Order._
import scdip.OrderMark._
import scdip.UnitType.{Army, Fleet}

// ref: http://www.floc.net/dpjudge/?page=Algorithm

case class OrderResolution(worldMap: WorldMap) {
  def exec(orderState: OrderState): OrderState = {
    orderState.evaluate(step1)
      .evaluate(step2)
      .evaluate(step3)
  }

  // Step 1. Mark All Invalid Convoy Orders
  private def step1(orderState: OrderState): OrderState = {
    orderState.fold {
      case (os, m: MoveOrder) if m.isNeighbour(worldMap) => os
      case (os, m: MoveOrder) => m.unitType match {
        case Army => if (m.canConvoy(worldMap, os.orders)) os else os.setMark(m, NoConvoy("no convoy path"))
        case Fleet => os.setMark(m, VoidMark("fleet can't jump"))
      }
      case (os, c: ConvoyOrder) => c.unitType match {
        case Army => os.setMark(c, VoidMark("army can't convoy"))
        case Fleet => c.findConvoyed(os.orders).fold(os.setMark(c, VoidMark("no convoy target"))) { m =>
          os.addConvoy(c, m)
        }
      }
      case (os, _) => os
    }
  }

  // Step 2. Mark All Invalid Move and Support Orders
  private def step2(orderState: OrderState): OrderState = {
    val orders = orderState.orders
    orderState.orderMatrix.foldLeft(orderState) {
      case (os, (s: SupportOrder, _)) if !s.reachSupport(worldMap) => os.setMark(s, VoidMark("not reach support target"))
      case (os, (s: SupportOrder, _)) if !s.existsSupportTarget(orders) => os.setMark(s, VoidMark("no support target"))
      case (os, (s: SupportHoldOrder, nm: NonMoveOrder)) if s.canSupport(nm) => os.addSupport(nm, s)
      case (os, (s: SupportMoveOrder, m: MoveOrder)) if s.canSupport(m) => os.addSupport(m, s)
      case (os, _) => os
    }
  }

  //  Step 3. Calculate Initial Combat Strengths
  private def step3(orderState: OrderState): OrderState = {
    orderState.moves.filter(_.isNeighbour(worldMap)).foldLeft(orderState) {
      case (os, m) => cutSupport(os, m)
    }
    //    (for {
    //      m <- orderState.moves.filter(_.isNeighbour(worldMap))
    //      s <- orderState.supports
    //    } yield (m, s)).foldLeft(orderState) {
    //      case (os, (m, so: SupportHoldOrder)) if so.src ~~ m.dst &&
    //        os.getMark(so).fold(true)(m => !m.isInstanceOf[VoidMark] && !m.isInstanceOf[CutMark]) &&
    //        so.power != m.power => os.setMark(so, CutMark()).delSupport(so)
    //      case (os, (m, so: SupportMoveOrder)) if so.src ~~ m.dst &&
    //        os.getMark(so).fold(true)(m => !m.isInstanceOf[VoidMark] && !m.isInstanceOf[CutMark]) &&
    //        so.power != m.power => os.setMark(so, CutMark()).delSupport(so).delNoHelpList(m, so)
    //      case (os, _) => os
    //    }
  }

  private def cutSupport(orderState: OrderState, moveOrder: MoveOrder): OrderState = {
    orderState.supports.foldLeft(orderState) {
      case (os, s) if moveOrder.dst ~~ s.src => if (orderState.getMark(s).fold(true)(m => !m.isInstanceOf[CutMark] && !m.isInstanceOf[VoidMark]) &&
        s.power != moveOrder.power &&
        (moveOrder.isNeighbour(worldMap) ||
          moveOrder.requireConvoy(worldMap) && orderState.supportTarget(s).fold(true) {
            case (c: ConvoyOrder) if orderState.isConvoyFleet(c) => false
            case _ => true
          })) s match {
        case sh: SupportHoldOrder => os.setMark(sh, CutMark()).delSupport(sh)
        case sm: SupportMoveOrder => os.setMark(sm, CutMark()).delSupport(sm).delNoHelpList(sm)
      } else {
        os
      }
      case (os, _) => os
    }
  }

  // Step 4. Mark Support Cuts Made by Convoyers and Mark Endangered Convoys
  private def step4(orderState: OrderState): OrderState = {
    // checkDisruption
    val newOS = checkDisruption(orderState)

    val newOS2 = newOS.convoyTargets.foldLeft(newOS) {
      case (os, m) if os.getMark(m).isEmpty => cutSupport(os, m).addConvoySucceeded(m)
      case (os, m) => os.setMark(m, ConvoyUnderAttack())
    }
    if (newOS2.convoySucceededSize > newOS.convoySucceededSize) {
      // TODO: recursive
    } else {
      // do nothing
      newOS2
    }
    (for {
      m <- newOS.convoyTargets
      s <- newOS.orders.flatMap {
        case (o: SupportOrder) => Option(o)
        case _ => None
      }
    } yield (m, s)).foldLeft(orderState) {
      case (os, (m, so: SupportHoldOrder)) if m.dst ~~ so.src &&
        os.getMark(so).fold(true)(m => !m.isInstanceOf[VoidMark] && !m.isInstanceOf[CutMark]) &&
        so.power != m.power &&
        os.supportTarget(so).fold(true) {
          case (o: ConvoyOrder) if os._convoyingArmies.contains(o) => false
          case _ => true
        } => os.setMark(so, CutMark()).delSupport(so)
      case (os, (m, so: SupportMoveOrder)) if m.dst ~~ so.src &&
        os.getMark(so).fold(false)(m => m.isInstanceOf[VoidMark] || m.isInstanceOf[CutMark]) &&
        so.power != m.power &&
        os.supportTarget(so).fold(true) {
          case (o: ConvoyOrder) if os._convoyingArmies.contains(o) => false
          case _ => true
        } => os.setMark(so, CutMark()).delSupport(so).delNoHelpList(so)
    }
  }

  private def checkDisruption(orderState: OrderState): OrderState = {
    orderState.convoyFleets.foldLeft(orderState) {
      case (os, c) => os.uniqueHighestSupportedOrder(c.src.province).flatMap(ho => if (ho.power != c.power) {
        os.convoyTarget(c).map(m => os.setMark(m, ConvoyEndangered()))
      } else {
        None
      }).getOrElse(os)
    }
  }

  // Step 5. Mark Convoy Disruptions And Support Cuts Made by Successful Convoys
  // Step 6. Mark Bounces Caused by Inability to Swap Places
  // Step 7. Mark Bounces Suffered by Understrength Attackers
  // Step 8. Mark Bounces Caused by Inability to Self-Dislodge
  // Step 9. Mark Supports Cut By Dislodgements


}

case class OrderState(orders: Seq[Order],
                      _orderMark: Map[Order, OrderMark] = Map.empty,
                      _supportCount: Map[SupportOrder, Order] = Map.empty,
                      _noHelps: Map[SupportMoveOrder, MoveOrder] = Map.empty,
                      _convoyingArmies: Map[ConvoyOrder, MoveOrder] = Map.empty,
                      _convoySucceeded: Set[MoveOrder] = Set.empty) {
  def evaluate(evaluator: OrderState => OrderState): OrderState = {
    evaluator(this)
  }


  def moves: Seq[MoveOrder] = orders.collect { case o: MoveOrder => o }

  def holds: Seq[HoldOrder] = orders.collect { case x: HoldOrder => x }

  def supports: Seq[SupportOrder] = orders.collect { case o: SupportOrder => o }

  private val combatList: Set[Province] = orders.map {
    case (m: MoveOrder) => m.dst
    case x => x.src
  }.map(_.province).toSet


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
    orders.map(o => if (_orderMark.get(o).isEmpty) o.success else o.failure)
  }

  // mark
  def setMark(order: Order, mark: OrderMark): OrderState = {
    copy(_orderMark = _orderMark.updated(order, mark))
  }

  def getMark(order: Order): Option[OrderMark] = _orderMark.get(order)

  // support
  def addSupport(nonMoveOrder: NonMoveOrder, supportHoldOrder: SupportHoldOrder): OrderState = {
    copy(_supportCount = _supportCount + (supportHoldOrder -> nonMoveOrder))
  }

  def addSupport(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): OrderState = {
    (if (orders.exists(o => o.src ~~ moveOrder.dst && o.power == moveOrder.power)) {
      addNoHelpList(moveOrder, supportMoveOrder)
    } else {
      this
    }).copy(_supportCount = _supportCount + (supportMoveOrder -> moveOrder))
  }

  def delSupport(supportOrder: SupportOrder): OrderState = {
    copy(_supportCount = _supportCount - supportOrder)
  }

  def supportTarget(supportOrder: SupportOrder): Option[Order] = _supportCount.get(supportOrder)

  def supportCount(order: Order): Int = {
    _supportCount.count { case (k, v) => v == order }
  }

  def uniqueHighestSupportedOrder(province: Province): Option[Order] = {
    orders.filter {
      case (m: MoveOrder) => m.dst ~~ province
      case (o) => o.src ~~ province
    }.map(o => o -> supportCount(o)).groupBy { case (o, s) => s }.toSeq.sortBy {
      case (s, o) => s
    }.reverse.headOption.flatMap {
      case (o, s) => if (s.size == 1) Option(s.head._1) else None
    }
  }

  // no help list
  def addNoHelpList(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): OrderState = {
    copy(_noHelps = _noHelps + (supportMoveOrder -> moveOrder))
  }

  def delNoHelpList(supportMoveOrder: SupportMoveOrder): OrderState = {
    copy(_noHelps = _noHelps - supportMoveOrder)
  }

  def getNoHelpList(moveOrder: MoveOrder): Seq[SupportMoveOrder] = {
    _noHelps.collect { case (sm, m) if m ~~ moveOrder => sm }.toSeq
  }

  // convoyingArmies
  def addConvoy(convoyOrder: ConvoyOrder, moveOrder: MoveOrder): OrderState = {
    copy(_convoyingArmies = _convoyingArmies + (convoyOrder -> moveOrder))
  }

  def convoyTarget(convoyOrder: ConvoyOrder): Option[MoveOrder] = _convoyingArmies.get(convoyOrder)

  def convoyFleets: Set[ConvoyOrder] = _convoyingArmies.keys.toSet

  def convoyTargets: Set[MoveOrder] = _convoyingArmies.values.toSet

  def isConvoyFleet(convoyOrder: ConvoyOrder): Boolean = _convoyingArmies.contains(convoyOrder)

  // convoy success
  def addConvoySucceeded(m: MoveOrder): OrderState = copy(_convoySucceeded = _convoySucceeded + m)

  def convoySucceededSize: Int = _convoySucceeded.size

}

trait OrderResult {
  def power: Power

  def order: OrderBase

  def gameUnit: GameUnit = GameUnit(power, order.unitType)

  def run[T](f: OrderBase => T): Option[T]

  def flatRun[T](f: OrderBase => Option[T]): Option[T]
}

case class SuccessResult(power: Power, order: OrderBase) extends OrderResult {
  override def run[T](f: (OrderBase) => T): Option[T] = Option(f(order))

  override def flatRun[T](f: (OrderBase) => Option[T]): Option[T] = f(order)
}

case class FailureResult(power: Power, order: OrderBase) extends OrderResult {
  override def run[T](f: (OrderBase) => T): Option[T] = None

  override def flatRun[T](f: (OrderBase) => Option[T]): Option[T] = None
}

object OrderMark {

  case class VoidMark(message: String = "") extends OrderMark

  case class NoConvoy(message: String = "") extends OrderMark

  case class CutMark(message: String = "") extends OrderMark

  case class ConvoyEndangered(message: String = "") extends OrderMark

  case class ConvoyUnderAttack(message: String = "") extends OrderMark

}


sealed trait OrderMark {
  def message: String
}


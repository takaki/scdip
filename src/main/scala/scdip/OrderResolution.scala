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
      .evaluate(step4to5)
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
    orderState.orderMatrix.foldLeft(orderState) {
      case (os, (s: SupportOrder, _)) if !s.reachSupport(worldMap) => os.setMark(s, VoidMark("not reach support target"))
      case (os, (s: SupportOrder, _)) if !s.existsSupportTarget(os.orders) => os.setMark(s, VoidMark("no support target"))
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
    orderState.evaluate(checkDisruption)
      .evaluate { newOS =>
        newOS.convoyAllTargets.foldLeft(newOS) {
          case (os, m) if os.getMark(m).isEmpty => cutSupport(os, m).evaluate { os =>
            if (os.isConvoySuccess(m)) {
              os
            } else {
              step4(os.addConvoySucceeded(m))
            }
          }
          case (os, m) => os.setMark(m, ConvoyUnderAttack())
        }
      }
  }

  private def checkDisruption(orderState: OrderState): OrderState = {
    orderState.convoyAllFleets.foldLeft(orderState) {
      case (os, c) => os.uniqueHighestSupportedOrder(c.src.province).flatMap(ho => if (ho.power != c.power) {
        os.convoyTarget(c).map(m => os.setMark(m, ConvoyEndangered()))
      } else {
        None
      }).getOrElse(os)
    }
  }

  // Step 5. Mark Convoy Disruptions And Support Cuts Made by Successful Convoys
  private def step5(orderState: OrderState): OrderState = {
    orderState.evaluate(checkDisruption).evaluate { os =>
      os.convoyAllTargets.foldLeft(os) { case (os1, m) =>
        if (os1.getMark(m).fold(false)(_.isInstanceOf[ConvoyEndangered])) {
          os1.setMark(m, NoConvoy()).delSupportTarget(m).evaluate {
            os2 => os2.convoyFleets(m).foldLeft(os2) { case (os3, c) => os3.setMark(c, NoConvoy()) }
          }
        } else {
          if (os1.getMark(m).fold(false)(_.isInstanceOf[ConvoyUnderAttack])) {
            os1.delMark(m).evaluate { os2 => cutSupport(os2, m) }.evaluate { os =>
              if (os.isConvoySuccess(m)) {
                os
              } else {
                step4to5(os.addConvoySucceeded(m))
              }
            }
          } else {
            os1
          }
        }
      }
    }
  }

  private def step4to5(orderState: OrderState): OrderState = {
    orderState.evaluate(step4).evaluate(step5)
  }

  // Step 6. Mark Bounces Caused by Inability to Swap Places
  private def step6(orderState: OrderState): OrderState = {
    (for {
      m <- orderState.moves.filter(m => m.isNeighbour(worldMap) && orderState.getMark(m).isEmpty)
      swapper <- orderState.moves if orderState.getMark(swapper).isEmpty && swapper.dst ~~ m.src && swapper.src ~~ m.dst && swapper.isNeighbour(worldMap)
    } yield (m, swapper)).foldLeft(orderState) {
      case (os, (m, sw)) => if (m.power == sw.power || os.supportCountNH(m) <= os.supportCount(sw)) {
        step6(bounce(os, m))
      } else if (m.power == sw.power || os.supportCountNH(sw) <= os.supportCount(m)) {
        step6(bounce(os, sw))
      } else {
        os
      }
    }
  }

  private def bounce(orderState: OrderState, moveOrder: MoveOrder): OrderState = {
    orderState.setMark(moveOrder, Bounce()).delNoHelpTarget(moveOrder).delSupportTarget(moveOrder)
  }

  private def step6to9(orderState: OrderState): OrderState = {
    orderState.evaluate(step6).evaluate(step7).evaluate(step8).evaluate(step9)
  }

  private def step6to8(orderState: OrderState): OrderState = {
    orderState.evaluate(step6).evaluate(step7).evaluate(step8)
  }

  private def step6to7(orderState: OrderState): OrderState = {
    orderState.evaluate(step6).evaluate(step7)
  }

  // Step 7. Mark Bounces Suffered by Understrength Attackers
  def step7(orderState: OrderState): OrderState = {
    // 要するにcombat listに載っている地域に移動しようとして十分なサポートがないとbounce
    orderState
  }

  // Step 8. Mark Bounces Caused by Inability to Self-Dislodge
  def step8(orderState: OrderState): OrderState = {
    orderState
  }

  // Step 9. Mark Supports Cut By Dislodgements
  def step9(orderState: OrderState): OrderState = {
    orderState
  }


}

// TODO: separate OrderState trait and OrdateStateImpl
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

  // combat list
  def combatList(province: Province): Seq[Order] = combatListMap.collect {
    case (o, p) if p == province => o
  }.toSeq

  private val combatListMap: Map[Order, Province] = orders.map {
    case (m: MoveOrder) => m -> m.dst.province
    case (o) => o -> o.src.province
  }.toMap

  def combatWinner = {
    combatListMap.values.toSet.flatMap { province =>
      val highest: Int = combatListMap.collect { case (o, p) if p == province => supportCount(o) }.toSeq.sorted.reverse.headOption.getOrElse(0)
      combatListMap.collect { case (m: MoveOrder, p) if p == province => m }.filter(o => supportCount(o) <= highest && getMark(o).isEmpty)
    }
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
    orders.map(o => if (_orderMark.get(o).isEmpty) o.success else o.failure)
  }

  // mark
  def setMark(order: Order, mark: OrderMark): OrderState = {
    copy(_orderMark = _orderMark + (order -> mark))
  }

  def delMark(m: MoveOrder): OrderState = copy(_orderMark = _orderMark - m)

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

  def delSupportTarget(m: MoveOrder): OrderState = {
    copy(_supportCount = _supportCount.filterNot { case (s, o) => o == m })
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

  def delNoHelpTarget(moveOrder: MoveOrder): OrderState = {
    copy(_noHelps = _noHelps.filterNot { case (s, o) => o == moveOrder })
  }

  def getNoHelpList(moveOrder: MoveOrder): Seq[SupportMoveOrder] = {
    _noHelps.collect { case (sm, m) if m ~~ moveOrder => sm }.toSeq
  }

  def supportCountNH(moveOrder: MoveOrder): Int = {
    _supportCount.count { case (k, v) => v == moveOrder } - getNoHelpList(moveOrder).size
  }

  // convoyingArmies
  def addConvoy(convoyOrder: ConvoyOrder, moveOrder: MoveOrder): OrderState = {
    copy(_convoyingArmies = _convoyingArmies + (convoyOrder -> moveOrder))
  }

  def convoyTarget(convoyOrder: ConvoyOrder): Option[MoveOrder] = _convoyingArmies.get(convoyOrder)

  def convoyAllFleets: Set[ConvoyOrder] = _convoyingArmies.keys.toSet

  def convoyAllTargets: Set[MoveOrder] = _convoyingArmies.values.toSet

  def convoyFleets(moveOrder: MoveOrder): Seq[ConvoyOrder] = _convoyingArmies.collect { case (c, m) if m == moveOrder => c }.toSeq

  def isConvoyFleet(convoyOrder: ConvoyOrder): Boolean = _convoyingArmies.contains(convoyOrder)

  // convoy success
  def addConvoySucceeded(m: MoveOrder): OrderState = copy(_convoySucceeded = _convoySucceeded + m)

  def isConvoySuccess(m: MoveOrder): Boolean = _convoySucceeded.contains(m)

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

  case class Bounce(message: String = "") extends OrderMark

}


sealed trait OrderMark {
  def message: String
}

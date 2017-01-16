package scdip

import scdip.Order._
import scdip.OrderMark._
import scdip.UnitType.{Army, Fleet}

// ref: http://www.floc.net/dpjudge/?page=Algorithm

case class MarkRecord(_orderMark: Map[Order, OrderMark] = Map.empty) {
  def setMark(order: Order, mark: OrderMark): MarkRecord = {
    copy(_orderMark = _orderMark + (order -> mark))
  }

  def delMark(m: MoveOrder): MarkRecord = copy(_orderMark = _orderMark - m)

  def getMark(order: Order): Option[OrderMark] = _orderMark.get(order)
}

case class SupportRecord(_supportCount: Map[SupportOrder, Order] = Map.empty,
                         _noHelps: Map[SupportMoveOrder, MoveOrder] = Map.empty) {
  def addSupport(order: Order, supportOrder: SupportOrder): SupportRecord = {
    copy(_supportCount = _supportCount + (supportOrder -> order))
  }

  def delSupport(supportOrder: SupportOrder): SupportRecord = {
    copy(_supportCount = _supportCount - supportOrder)
  }

  def delSupportTarget(m: MoveOrder): SupportRecord = {
    copy(_supportCount = _supportCount.filterNot { case (s, o) => o == m })
  }

  def supportTarget(supportOrder: SupportOrder): Option[Order] = _supportCount.get(supportOrder)

  def supportCount(order: Order): Int = {
    _supportCount.count { case (k, v) => v == order }
  }


  def addNoHelpList(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): SupportRecord = {
    copy(_noHelps = _noHelps + (supportMoveOrder -> moveOrder))
  }

  def delNoHelpList(supportMoveOrder: SupportMoveOrder): SupportRecord = {
    copy(_noHelps = _noHelps - supportMoveOrder)
  }

  def delNoHelpTarget(moveOrder: MoveOrder): SupportRecord = {
    copy(_noHelps = _noHelps.filterNot { case (s, o) => o == moveOrder })
  }

  def getNoHelpList(moveOrder: MoveOrder): Seq[SupportMoveOrder] = {
    _noHelps.collect { case (sm, m) if m ~~ moveOrder => sm }.toSeq
  }

  def supportCountNH(moveOrder: MoveOrder): Int = {
    _supportCount.count { case (k, v) => v == moveOrder } - getNoHelpList(moveOrder).size
  }


}

object OrderState {
  val steps: Seq[(OrderState) => OrderState] = List(step1, step2, step3, step4to5, step6to9)

  // Step 1. Mark All Invalid Convoy Orders
  private def step1(orderState: OrderState): OrderState = {
    orderState.orders.foldLeft(orderState) {
      case (os, m: MoveOrder) if m.isNeighbour(orderState.worldMap) => os
      case (os, m: MoveOrder) if m.unitType == Army => if (m.canConvoy(orderState.worldMap, os.orders)) os else os.setMarkRecord(os.markRecord.setMark(m, NoConvoy("no convoy path")))
      case (os, m: MoveOrder) if m.unitType == Fleet => os.setMark(m, VoidMark("fleet can't jump"))
      case (os, c: ConvoyOrder) if c.unitType == Army => os.setMark(c, VoidMark("army can't convoy"))
      case (os, c: ConvoyOrder) if c.unitType == Fleet => c.findConvoyTarget(os.moves).fold(os.setMark(c, VoidMark("no convoy target"))) { m =>
        os.addConvoy(c, m)
      }
      case (os, _) => os
    }
  }

  // Step 2. Mark All Invalid Move and Support Orders
  private def step2(orderState: OrderState): OrderState = {
    (for {
      s <- orderState.supports
      o <- orderState.orders
    } yield (s, o)).foldLeft(orderState) {
      case (os, (s, _)) if !s.reachSupport(orderState.worldMap) => os.setMark(s, VoidMark("not reach support target"))
      case (os, (s, _)) if !s.existsSupportTarget(os.orders) => os.setMark(s, VoidMark("no support target"))
      case (os, (s: SupportHoldOrder, nm: NonMoveOrder)) if s.canSupport(nm) => os.setSupportRecord(orderState.supportRecord.addSupport(nm, s))
      case (os, (s: SupportMoveOrder, m: MoveOrder)) if s.canSupport(m) => os.setSupportRecord(
        (if (os.orders.exists(o => o.src ~~ m.dst && o.power == m.power)) {
          os.supportRecord.addNoHelpList(m, s)
        } else {
          os.supportRecord
        }).addSupport(m, s))
      case (os, _) => os
    }
  }

  //  Step 3. Calculate Initial Combat Strengths
  private def step3(orderState: OrderState): OrderState = {
    orderState.moves.filter(_.isNeighbour(orderState.worldMap)).foldLeft(orderState) {
      case (os, m) => cutSupport(os, m)
    }
  }

  private def cutSupport(orderState: OrderState, moveOrder: MoveOrder): OrderState = {
    orderState.supports.foldLeft(orderState) {
      case (os, s) if moveOrder.dst ~~ s.src => if (os.markRecord.getMark(s).fold(true)(m => !m.isInstanceOf[CutMark] && !m.isInstanceOf[VoidMark]) &&
        s.power != moveOrder.power &&
        (moveOrder.isNeighbour(orderState.worldMap) ||
          moveOrder.requireConvoy(orderState.worldMap) && os.supportRecord.supportTarget(s).fold(true) {
            case (c: ConvoyOrder) if os.isConvoyFleet(c) => false
            case _ => true
          })) s match {
        case sh: SupportHoldOrder => os.setMark(sh, CutMark()).setSupportRecord(os.supportRecord.delSupport(sh))
        case sm: SupportMoveOrder => os.setMark(sm, CutMark()).setSupportRecord(os.supportRecord.delSupport(sm).delNoHelpList(sm))
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
          case (os, m) if os.markRecord.getMark(m).isEmpty => cutSupport(os, m).evaluate { os =>
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
        if (os1.markRecord.getMark(m).fold(false)(_.isInstanceOf[ConvoyEndangered])) {
          os1.setMark(m, NoConvoy()).setSupportRecord(os1.supportRecord.delSupportTarget(m)).evaluate {
            os2 => os2.convoyFleets(m).foldLeft(os2) { case (os3, c) => os3.setMark(c, NoConvoy()) }
          }
        } else {
          if (os1.markRecord.getMark(m).fold(false)(_.isInstanceOf[ConvoyUnderAttack])) {
            os1.setMarkRecord(os1.markRecord.delMark(m)).evaluate { os2 => cutSupport(os2, m) }.evaluate { os =>
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

  def step4to5(orderState: OrderState): OrderState = {
    step5(step4(orderState))
  }

  // Step 6. Mark Bounces Caused by Inability to Swap Places
  private def step6(orderState: OrderState): OrderState = {
    (for {
      m <- orderState.moves.filter(m => m.isNeighbour(orderState.worldMap) && orderState.markRecord.getMark(m).isEmpty)
      swapper <- orderState.moves if orderState.markRecord.getMark(swapper).isEmpty && swapper.dst ~~ m.src && swapper.src ~~ m.dst && swapper.isNeighbour(orderState.worldMap)
    } yield (m, swapper)).foldLeft(orderState) {
      case (os, (m, sw)) => if (m.power == sw.power || os.supportRecord.supportCountNH(m) <= os.supportRecord.supportCount(sw)) {
        step6(bounce(os, m, "step6"))
      } else if (m.power == sw.power || os.supportRecord.supportCountNH(sw) <= os.supportRecord.supportCount(m)) {
        step6(bounce(os, sw, "step6"))
      } else {
        os
      }
    }
  }

  private def bounce(orderState: OrderState, moveOrder: MoveOrder, message: String = ""): OrderState = {
    orderState.setMark(moveOrder, Bounce(message)).setSupportRecord(orderState.supportRecord.delNoHelpTarget(moveOrder).delSupportTarget(moveOrder))
  }

  private def step6to9(orderState: OrderState): OrderState = {
    step9(step8(step7(step6(orderState))))
  }

  private def step6to8(orderState: OrderState): OrderState = {
    step8(step7(step6(orderState)))
  }

  private def step6to7(orderState: OrderState): OrderState = {
    step7(step6(orderState))
  }

  // Step 7. Mark Bounces Suffered by Understrength Attackers
  private def step7(orderState: OrderState): OrderState = {
    orderState.combatListMap.values.toSet.flatMap((province: Province) => {
      val highest: Int = orderState.combatListMap.collect { case (o, p) if p == province => orderState.supportRecord.supportCount(o) }.toSeq.sorted.reverse.headOption.getOrElse(0)
      val tryMove = orderState.combatListMap.collect { case (m: MoveOrder, p) if p == province => m }
      if (tryMove.count(m => orderState.supportRecord.supportCount(m) == highest && orderState.markRecord.getMark(m).isEmpty) > 1) {
        tryMove
      } else {
        tryMove.filter(m => orderState.supportRecord.supportCount(m) < highest && orderState.markRecord.getMark(m).isEmpty)
      }
    }).foldLeft(orderState) {
      case (os, m) => step6to7(bounce(os, m, "step7"))
    }
  }

  // Step 8. Mark Bounces Caused by Inability to Self-Dislodge
  private def step8(orderState: OrderState): OrderState = {
    orderState.combatListMap.values.toSet.flatMap((province: Province) => {
      val highest: Int = orderState.combatListMap.collect { case (o, p) if p == province => orderState.supportRecord.supportCount(o) }.toSeq.sorted.reverse.headOption.getOrElse(0)
      val highestOrders = orderState.combatListMap.collect { case (m: MoveOrder, p) if p == province => m }.filter(o => orderState.supportRecord.supportCount(o) == highest)
      if (highestOrders.size == 1) {
        highestOrders.collect {
          case (m: MoveOrder) if orderState.markRecord.getMark(m).isEmpty => m
        }
      } else {
        Seq.empty
      }
    }).foldLeft(orderState) {
      case (os, m) => orderState.orders.find(o => o.src ~~ m.dst).fold(os) {
        case (m: MoveOrder) if orderState.markRecord.getMark(m).isEmpty => os
        case o if o.power == m.power => step6to8(bounce(os, m, "step8"))
        case o if orderState.supportRecord.supportCountNH(m) > 0 => os // TODO: no help support
        case _ => os
      }
    }
  }

  // Step 9. Mark Supports Cut By Dislodgements
  private def step9(orderState: OrderState): OrderState = {
    orderState
    // this.moves.filter(o => getMark(o).isEmpty).foldLeft(this) { case (os, m) => cutSupport(os, m).step6to9 } // TODO after success cutSupport
  }

}

case class OrderState(orders: Seq[Order],
                      worldMap: WorldMap,
                      markRecord: MarkRecord = MarkRecord(),
                      supportRecord: SupportRecord = SupportRecord(),
                      _convoyingArmies: Map[ConvoyOrder, MoveOrder] = Map.empty,
                      _convoySucceeded: Set[MoveOrder] = Set.empty) {

  def resolve: OrderState = {
    OrderState.steps.foldLeft(this) {
      case (os, f) => f(os)
    }
  }


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
    orders.map(o => if (markRecord.getMark(o).isEmpty) o.success else o.failure)
  }

  // mark
  def setMarkRecord(markRecord: MarkRecord): OrderState = {
    copy(markRecord = markRecord)
  }

  def setMark(order: Order, mark: OrderMark): OrderState = {
    setMarkRecord(markRecord = markRecord.setMark(order, mark))
  }

  // support
  def setSupportRecord(supportRecord: SupportRecord): OrderState = {
    copy(supportRecord = supportRecord)
  }

  def uniqueHighestSupportedOrder(province: Province): Option[Order] = {
    orders.filter {
      case (m: MoveOrder) => m.dst ~~ province
      case (o) => o.src ~~ province
    }.map(o => o -> supportRecord.supportCount(o)).groupBy { case (o, s) => s }.toSeq.sortBy {
      case (s, o) => s
    }.reverse.headOption.flatMap {
      case (o, s) => if (s.size == 1) Option(s.head._1) else None
    }
  }

  // no help list
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

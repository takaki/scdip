package scdip

import scdip.Order._
import scdip.OrderMark._
import scdip.OrderState._
import scdip.UnitType.{Army, Fleet}

// ref: http://www.floc.net/dpjudge/?page=Algorithm

object OrderState {
  def steps(orderState: OrderState): OrderState = {
    step6to9(step4to5(step3(step2(step1(orderState)))))
  }

  // Step 1. Mark All Invalid Convoy Orders
  private def step1(orderState: OrderState): OrderState = {
    def step1moves(orderState: OrderState): OrderState = {
      orderState.moves.foldLeft(orderState) {
        case (os, m) if m.src ~~ m.dst => os.setMarkRecord(os.markRecord.setMark(m, VoidMark("same place")))
        case (os, m) if !m.requireConvoy(orderState.worldMap) => os
        case (os, m) if m.unitType == Army => if (m.canConvoy(orderState.worldMap, os.orders)) os else os.setMarkRecord(os.markRecord.setMark(m, NoConvoy("no convoy path")))
        case (os, m) if m.unitType == Fleet => os.setMark(m, VoidMark("fleet can't jump"))
        case (os, _) => os
      }
    }

    def step1convoys(orderState: OrderState): OrderState = {
      orderState.convoys.foldLeft(orderState) {
        case (os, c) if c.unitType == Army => os.setMark(c, VoidMark("army can't convoy"))
        case (os, c) if c.unitType == Fleet => c.findConvoyTarget(os.moves).fold(os.setMark(c, VoidMark("no convoy target"))) { m =>
          if (os.notMarked(m)) os.addConvoy(c, m) else os.setMark(c, VoidMark("no convoy target"))
        }
        case (os, _) => os
      }
    }

    (step1moves _).andThen(step1convoys)(orderState)
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
    orderState.moves.filterNot(_.requireConvoy(orderState.worldMap)).foldLeft(orderState) {
      case (os, m) => cutSupport(os, m)
    }
  }

  private def cutSupport(orderState: OrderState, moveOrder: MoveOrder, after: ((OrderState) => OrderState) = identity): OrderState = {
    def cond0(os: OrderState, s: SupportOrder): Boolean = {
      os.markRecord.getMark(s).fold(true)(m => !m.isInstanceOf[CutMark] && !m.isInstanceOf[VoidMark])
    }

    orderState.supports.foldLeft(orderState) {
      case (os, s) if moveOrder.dst ~~ s.src => if (cond0(os, s) && s.power != moveOrder.power &&
        (!moveOrder.requireConvoy(orderState.worldMap) ||
          moveOrder.requireConvoy(orderState.worldMap) && os.supportRecord.supportTarget(s).fold(true) {
            case (c: ConvoyOrder) if os.isConvoyFleet(c) => false
            case _ => true
          })) s match {
        case sh: SupportHoldOrder => after(os.setMark(sh, CutMark()).setSupportRecord(os.supportRecord.delSupport(sh)))
        case sm: SupportMoveOrder => after(os.setMark(sm, CutMark()).setSupportRecord(os.supportRecord.delSupport(sm).delNoHelpList(sm)))
      } else {
        os
      }
      case (os, _) => os
    }
  }

  // Step 4. Mark Support Cuts Made by Convoyers and Mark Endangered Convoys
  private def step4(orderState: OrderState): OrderState = {

    def impl(orderState: OrderState): OrderState = {
      orderState.convoyAllTargets.foldLeft(orderState) {
        case (os, m) if os.notMarked(m) => impl2(cutSupport(os, m), m)
        case (os, m) => os.setMark(m, ConvoyUnderAttack())
      }

    }

    def impl2(orderState: OrderState, moveOrder: MoveOrder): OrderState = {
      if (orderState.isConvoySuccess(moveOrder)) {
        orderState
      } else {
        step4(orderState.addConvoySucceeded(moveOrder))
      }
    }

    (checkDisruption _).andThen(impl)(orderState)
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
    def impl0(orderState: OrderState) = {
      orderState.convoyAllTargets.foldLeft(orderState) { case (os, m) =>
        if (os.markRecord.getMark(m).fold(false)(_.isInstanceOf[ConvoyEndangered])) {
          impl1(os.setMark(m, NoConvoy()).setSupportRecord(os.supportRecord.delSupportTarget(m)), m)
        } else {
          if (os.markRecord.getMark(m).fold(false)(_.isInstanceOf[ConvoyUnderAttack])) {
            (cutSupport(_: OrderState, m)).andThen(impl2(_: OrderState, m))(os.setMarkRecord(os.markRecord.delMark(m)))
          } else {
            os
          }
        }
      }
    }

    def impl1(orderState: OrderState, m: MoveOrder): OrderState = {
      orderState.convoyFleets(m).foldLeft(orderState) { case (os, c) => os.setMark(c, NoConvoy()) }
    }

    def impl2(orderState: OrderState, m: MoveOrder): OrderState = {
      if (orderState.isConvoySuccess(m)) {
        orderState
      } else {
        step4to5(orderState.addConvoySucceeded(m))
      }
    }

    (checkDisruption _).andThen(impl0)(orderState)
  }

  def step4to5(orderState: OrderState): OrderState = {
    step5(step4(orderState))
  }

  // Step 6. Mark Bounces Caused by Inability to Swap Places
  private def step6(orderState: OrderState): OrderState = {
    (for {
      m <- orderState.moves.filter(m => !m.requireConvoy(orderState.worldMap) && orderState.notMarked(m))
      swapper <- orderState.moves if orderState.notMarked(swapper) && swapper.dst ~~ m.src && swapper.src ~~ m.dst && !swapper.requireConvoy(orderState.worldMap)
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
    val bounced = orderState.combatListRecord.provinces.flatMap(province => {
      val highest: Int = orderState.highestSupportCount(province)
      val tryMoves: Seq[MoveOrder] = orderState.combatListRecord.orders(province).collect { case (m: MoveOrder) => m }
      if (tryMoves.count(m => orderState.supportRecord.supportCount(m) == highest && orderState.notMarked(m)) > 1) {
        tryMoves
      } else {
        tryMoves.filter(m => orderState.supportRecord.supportCount(m) < highest && orderState.notMarked(m))
      }
    })
    if (bounced.isEmpty) {
      orderState
    } else {
      step6to7(bounced.foldLeft(orderState) { case (os, m) => bounce(os, m, "step7") })
    }
  }

  // Step 8. Mark Bounces Caused by Inability to Self-Dislodge
  private def step8(orderState: OrderState): OrderState = {
    orderState.combatListRecord.provinces.flatMap(province => {
      val highest: Int = orderState.highestSupportCount(province)
      val highestOrders = orderState.combatListRecord.orders(province).collect { case (m: MoveOrder) if orderState.supportRecord.supportCount(m) == highest => m }
      if (highestOrders.size == 1) {
        highestOrders.collect {
          case (m: MoveOrder) if orderState.notMarked(m) => m
        }
      } else {
        Seq.empty
      }
    }).foldLeft(orderState) {
      case (os, m) => orderState.orders.find(o => o.src ~~ m.dst).fold(os) {
        case (m: MoveOrder) if orderState.notMarked(m) => os
        case o if o.power == m.power => step6to8(bounce(os, m, "step8"))
        case o if orderState.supportRecord.supportCountNH(m) > 0 => os
        case _ => os
      }
    }
  }

  // Step 9. Mark Supports Cut By Dislodgements
  private def step9(orderState: OrderState): OrderState = {
    orderState.moves.filter(o => orderState.notMarked(o)).foldLeft(orderState) { case (os, m) => cutSupport(os, m, step6to9) }
  }


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

  case class CombatListRecord(_orders: Seq[Order]) {
    private val map = _orders.map {
      case (m: MoveOrder) => m -> m.dst.province
      case (o) => o -> o.src.province
    }.toMap

    def orders(province: Province): Seq[Order] = map.collect {
      case (o, p) if p == province => o
    }.toSeq

    def provinces: Seq[Province] = {
      map.values.toSeq.distinct
    }

  }

  case class ConvoyingArmies(_convoyingArmies: Map[ConvoyOrder, MoveOrder] = Map.empty) {
    def addConvoy(convoyOrder: ConvoyOrder, moveOrder: MoveOrder): ConvoyingArmies = {
      copy(_convoyingArmies + (convoyOrder -> moveOrder))
    }

    def convoyTarget(convoyOrder: ConvoyOrder): Option[MoveOrder] = _convoyingArmies.get(convoyOrder)

    def convoyAllFleets: Set[ConvoyOrder] = _convoyingArmies.keys.toSet

    def convoyAllTargets: Set[MoveOrder] = _convoyingArmies.values.toSet

    def convoyFleets(moveOrder: MoveOrder): Seq[ConvoyOrder] = _convoyingArmies.collect { case (c, m) if m == moveOrder => c }.toSeq

    def isConvoyFleet(convoyOrder: ConvoyOrder): Boolean = _convoyingArmies.contains(convoyOrder)

  }

  case class ConvoySucceeded(_convoySucceeded: Set[MoveOrder] = Set.empty) {
    def addConvoySucceeded(m: MoveOrder): ConvoySucceeded = copy(_convoySucceeded = _convoySucceeded + m)

    def isConvoySuccess(m: MoveOrder): Boolean = _convoySucceeded.contains(m)
  }


}


case class OrderState(orders: Seq[Order],
                      worldMap: WorldMap,
                      markRecord: MarkRecord = MarkRecord(),
                      supportRecord: SupportRecord = SupportRecord(),
                      convoyingArmies: ConvoyingArmies = ConvoyingArmies(),
                      convoySucceeded: ConvoySucceeded = ConvoySucceeded()) {

  def notMarked(order: Order): Boolean = markRecord.getMark(order).isEmpty


  def addConvoySucceeded(moveOrder: MoveOrder): OrderState = copy(convoySucceeded = convoySucceeded.addConvoySucceeded(moveOrder))


  def isConvoySuccess(moveOrder: MoveOrder): Boolean = convoySucceeded.isConvoySuccess(moveOrder)


  def resolve: OrderState = {
    OrderState.steps(this)
  }

  def moves: Seq[MoveOrder] = orders.collect { case o: MoveOrder => o }

  def holds: Seq[HoldOrder] = orders.collect { case o: HoldOrder => o }

  def supports: Seq[SupportOrder] = orders.collect { case o: SupportOrder => o }

  def convoys: Seq[ConvoyOrder] = orders.collect { case o: ConvoyOrder => o }

  def results: Seq[OrderResult] = {
    orders.map(o => if (notMarked(o)) o.success else o.failure)
  }

  // combat list
  val combatListRecord = CombatListRecord(orders) // TODO: instance

  // mark
  def setMarkRecord(markRecord: MarkRecord): OrderState = {
    copy(markRecord = markRecord)
  }

  def setMark(order: Order, mark: OrderMark): OrderState = {
    setMarkRecord(markRecord.setMark(order, mark))
  }

  // support
  def setSupportRecord(supportRecord: SupportRecord): OrderState = {
    copy(supportRecord = supportRecord)
  }

  def uniqueHighestSupportedOrder(province: Province): Option[Order] = {
    val (_, os) = orders.filter {
      case (m: MoveOrder) => m.dst ~~ province
      case (o) => o.src ~~ province
    }.map(o => o -> supportRecord.supportCount(o)).groupBy {
      case (_, sc) => sc
    }.maxBy {
      case (sc, _) => sc
    }
    if (os.size == 1) Option(os.head._1) else None
  }

  def highestSupportCount(province: Province): Int = {
    combatListRecord.orders(province).map { o => supportRecord.supportCount(o) }.reduceOption(_ max _).getOrElse(0)
  }

  // no help list
  // convoyingArmies
  def addConvoy(convoyOrder: ConvoyOrder, moveOrder: MoveOrder): OrderState = {
    copy(convoyingArmies = convoyingArmies.addConvoy(convoyOrder, moveOrder))
  }

  def convoyTarget(convoyOrder: ConvoyOrder): Option[MoveOrder] = convoyingArmies.convoyTarget(convoyOrder)

  def convoyAllFleets: Set[ConvoyOrder] = convoyingArmies.convoyAllFleets

  def convoyAllTargets: Set[MoveOrder] = convoyingArmies.convoyAllTargets

  def convoyFleets(moveOrder: MoveOrder): Seq[ConvoyOrder] = convoyingArmies.convoyFleets(moveOrder)

  def isConvoyFleet(convoyOrder: ConvoyOrder): Boolean = convoyingArmies.isConvoyFleet(convoyOrder)

  // convoy success

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

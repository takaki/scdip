package scdip

import scdip.Order._
import scdip.OrderMark._
import scdip.OrderState._
import scdip.UnitType.{Army, Fleet}

// ref: http://www.floc.net/dpjudge/?page=Algorithm

object OrderState {
  def steps(orderState: OrderState): OrderState = {
    step10(step6to9(step4to5(step3(step2(step1(orderState))))))
  }

  // Step 1. Mark All Invalid Convoy Orders
  private def step1(orderState: OrderState): OrderState = {
    def step1moves(orderState: OrderState): OrderState = {
      orderState.moves.foldLeft(orderState) {
        case (os, m) if m.src ~~ m.dst => os.setMark(m, VoidMark("same province"))
        case (os, m) if !m.requireConvoy(orderState.worldMap) => os
        case (os, m) if m.unitType == Army => if (m.canConvoy(orderState.worldMap, os.orders)) os else os.setMark(m, NoConvoy("no convoy path"))
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
    def check0(orderState: OrderState): OrderState = {
      orderState.supports.filter(orderState.notMarked(_)).foldLeft(orderState) {
        case (os, s) =>
          if (!s.reachSupport(orderState.worldMap)) {
            os.setMark(s, VoidMark("not reach support target"))
          } else {
            os
          }
      }
    }

    def check1(orderState: OrderState): OrderState = {
      orderState.supports.filter(orderState.notMarked(_)).foldLeft(orderState) {
        case (os, s: SupportHoldOrder) =>
          s.findSupportTarget(os.orders).fold(os.setMark(s, VoidMark("no support target(sh)"))) {
            case (nm: NonMoveOrder) => os.addSupport(nm, s)
            case (m: MoveOrder) if !os.notMarked(m) => os.addSupport(m, s)
            case _ => os
          }
        case (os, s: SupportMoveOrder) =>
          s.findSupportTarget(os.orders.filter(o => os.notMarked(o))).fold(os.setMark(s, VoidMark("no support target(sm)"))) {
            case (m: MoveOrder) => if (os.orders.exists(o => o.src ~~ m.dst && o.power == s.power)) {
              os.addSupport(m, s).addNoHelpList(m, s)
            } else {
              os.addSupport(m, s)
            }
            case _ => os
          }
      }
    }

    (check0 _).andThen(check1)(orderState)
  }

  //  Step 3. Calculate Initial Combat Strengths
  private def step3(orderState: OrderState): OrderState = {
    orderState.moves.filterNot(_.requireConvoy(orderState.worldMap)).foldLeft(orderState) {
      case (os, m) => cutSupport(os, m, "step3")
    }.copy(_combatListRecord = orderState.orders.foldLeft(orderState._combatListRecord) {
      case (cl, m: MoveOrder) if orderState.notMarked(m) => cl.add(m.dst.province, m)
      case (cl, m: MoveOrder) => cl.add(m.src.province, m)
      case (cl, o) => cl.add(o.src.province, o)
    })
  }

  private def cutSupport(orderState: OrderState, moveOrder: MoveOrder, message: String, after: ((OrderState) => OrderState) = identity, inStep9: Boolean = false): OrderState = {
    def cond1(os: OrderState, s: SupportOrder): Boolean = {
      os.getMark(s).fold(true)(m => !m.isInstanceOf[CutMark] && !m.isInstanceOf[VoidMark])
    }

    orderState.supports.foldLeft(orderState) {
      case (os, s) if moveOrder.dst ~~ s.src => if (cond1(os, s) && s.power != moveOrder.power &&
        (!moveOrder.requireConvoy(orderState.worldMap) ||
          moveOrder.requireConvoy(orderState.worldMap) && os.supportTarget(s).fold(true) {
            case (c: ConvoyOrder) if os.isConvoyFleet(c) => false
            case _ => true
          })) s match {
        case sh: SupportHoldOrder => after(os.setMark(sh, CutMark(s"$message; $moveOrder")).delSupport(sh))
        case sm: SupportMoveOrder => if (!inStep9 && sm.to ~~ moveOrder.src) {
          os
        } else {
          after(os.setMark(sm, CutMark(s"$message; $moveOrder")).delSupport(sm).delNoHelpList(sm))
        }
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
        case (os, m) if os.notMarked(m) => impl2(cutSupport(os, m, "step4-impl"), m)
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
    orderState.convoyGroups.foldLeft(orderState) {
      case (os, (m, cs)) =>
        val safe = cs.filter(c => os.uniqueHighestSupportedOrder(c.src.province).fold(true)(ho => ho.power == c.power))
        if (m.canConvoy(os.worldMap, safe.toSeq)) {
          os
        } else {
          os.setMark(m, ConvoyEndangered())
        }
    }
//    orderState.convoyAllFleets.foldLeft(orderState) {
//      case (os, c) => os.uniqueHighestSupportedOrder(c.src.province).flatMap(ho => if (ho.power != c.power) {
//        os.convoyTarget(c).map(m => os.setMark(m, ConvoyEndangered()))
//      } else {
//        None
//      }).getOrElse(os)
//    }
  }

  // Step 5. Mark Convoy Disruptions And Support Cuts Made by Successful Convoys
  private def step5(orderState: OrderState): OrderState = {
    def impl0(orderState: OrderState) = {
      orderState.convoyAllTargets.foldLeft(orderState) { case (os, m) =>
        if (os._markRecord.getMark(m).fold(false)(_.isInstanceOf[ConvoyEndangered])) {
          impl1(os.setMark(m, NoConvoy("step5-endangered")).delSupportTarget(m).delCombatList(m).addCombatList(m.src.province, m), m)
        } else {
          if (os._markRecord.getMark(m).fold(false)(_.isInstanceOf[ConvoyUnderAttack])) {
            (cutSupport(_: OrderState, m, "step5-1")).andThen(impl2(_: OrderState, m))(os.delMark(m))
          } else {
            os
          }
        }
      }
    }

    def impl1(orderState: OrderState, m: MoveOrder): OrderState = {
      orderState.convoyFleets(m).foldLeft(orderState) { case (os, c) => os.setMark(c, NoConvoy("step5-target-endangered")) }
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
      case (os, (m, sw)) => if (m.power == sw.power || os.supportCountNH(m) <= os.supportCount(sw)) {
        step6(bounce(os, m, s"step6 by $sw"))
      } else if (m.power == sw.power || os.supportCountNH(sw) <= os.supportCount(m)) {
        step6(bounce(os, sw, s"step6 by $m"))
      } else {
        os
      }
    }
  }

  private def bounce(orderState: OrderState, moveOrder: MoveOrder, message: String = ""): OrderState = {
    orderState.setMark(moveOrder, Bounce(message))
      //      .delNoHelpTarget(moveOrder).delSupportTarget(moveOrder)
      .addCombatList(moveOrder.src.province, moveOrder)
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
    val bounced = orderState._combatListRecord.provinces.flatMap(province => {
      orderState._combatListRecord.orders(province).collect { case (m: MoveOrder) if orderState.notMarked(m) => m }.filter(m =>
        orderState._combatListRecord.orders(province).filter(o => o != m).exists {
          case (o: MoveOrder) if orderState.getMark(o).fold(false)(_.isInstanceOf[Bounce]) && o.src.province == province => 0 >= orderState.supportCount(m)
          case (o) => orderState.supportCount(o) >= orderState.supportCount(m)
        }
      )
    })
    if (bounced.isEmpty) {
      orderState
    } else {
      step6to7(bounced.foldLeft(orderState) { case (os, m) => bounce(os, m, "step7") })
    }
  }

  // Step 8. Mark Bounces Caused by Inability to Self-Dislodge
  private def step8(orderState: OrderState): OrderState = {
    orderState._combatListRecord.provinces.flatMap(province => {
      val highest: Int = orderState.highestSupportCount(province)
      val highestOrders = orderState._combatListRecord.orders(province).collect {
        case (m: MoveOrder) if orderState.supportCount(m) == highest => m
      }
      if (highestOrders.size == 1) {
        highestOrders.collect {
          case (m: MoveOrder) if orderState.notMarked(m) => m
        }
      } else {
        Seq.empty
      }
    }).foldLeft(orderState) {
      case (os, m) => os.orders.find(h => h.src ~~ m.dst).fold(os) {
        case (o: MoveOrder) if orderState.notMarked(o) => os
        case nm if nm.power == m.power => step6to8(bounce(os, m, "step8 self-attack"))
        case nm => if (os.supportCountNH(m) > os._combatListRecord.orders(m.dst.province).filterNot(o => o == m).map(o => os.supportCount(o)).reduceOption(_ max _).getOrElse(0)) {
          os
        } else {
          step6to8(bounce(os, m, "step8-NH"))
        }
      }
    }
  }

  // Step 9. Mark Supports Cut By Dislodgements
  private def step9(orderState: OrderState): OrderState = {
    orderState.moves.filter(o => orderState.notMarked(o)).foldLeft(orderState) {
      case (os, m) => cutSupport(os, m, "step9", step6to9, inStep9 = true)
    }
  }

  //Step 10. Move Units That Did Not Bounce
  private def step10(orderState: OrderState): OrderState = {
    @scala.annotation.tailrec
    def unbounce(orderState: OrderState, province: Province): OrderState = {
      val orders = orderState._combatListRecord.orders(province)
      if (orders.isEmpty) {
        orderState
      } else {
        val (_, maximum) = orders.map(o => (o, orderState.supportCount(o))).groupBy { case (_, sc) => sc }.maxBy { case (sc, _) => sc }
        if (maximum.size == 1) {
          val (max, _) = maximum.head
          if (orderState.getMark(max).fold(false)(_.isInstanceOf[Bounce])) {
            val os2 = orderState.delMark(max)
            if (os2.isDislodged(max)) {
              os2.delDislodged(max)
            } else {
              unbounce(os2.delCombatList(province, max), province)
            }
          } else {
            orderState
          }
        } else {
          orderState
        }
      }
    }

    val dislodgedPairs = orderState.moves.filter(orderState.notMarked).flatMap(m => orderState.orders.find {
      case (o: NonMoveOrder) if m.dst ~~ o.src => true
      case (o) if !orderState.notMarked(o) && m.dst ~~ o.src => true
      case _ => false
    }.map(o => (m, o)))
    val os2 = dislodgedPairs.foldLeft(orderState) {
      case (os, (m, o: MoveOrder)) if !m.requireConvoy(os.worldMap) && !o.requireConvoy(os.worldMap) && m.src ~~ o.dst =>
        os.delCombatList(o).addDislodged(o)
      case (os, _) => os
    }
    dislodgedPairs.foldLeft(os2) {
      case (os, (m, _)) => unbounce(os, m.src.province)
    }
    // retreat list
  }

  case class MarkRecord(_orderMark: Map[Order, OrderMark] = Map.empty) {
    def setMark(order: Order, mark: OrderMark): MarkRecord = {
      copy(_orderMark = _orderMark + (order -> mark))
    }

    def delMark(o: Order): MarkRecord = copy(_orderMark = _orderMark - o)

    def getMark(order: Order): Option[OrderMark] = _orderMark.get(order)
  }

  case class SupportRecord(_holdCount: Map[SupportHoldOrder, Order] = Map.empty,
                           _moveCount: Map[SupportMoveOrder, MoveOrder] = Map.empty,
                           _noHelps: Map[SupportMoveOrder, MoveOrder] = Map.empty) {
    def addSupport(order: Order, supportOrder: SupportOrder): SupportRecord = (order, supportOrder) match {
      case (o, s: SupportHoldOrder) => copy(_holdCount = _holdCount + (s -> o))
      case (m: MoveOrder, s: SupportMoveOrder) => copy(_moveCount = _moveCount + (s -> m))
      case _ => this
    }

    def delSupport(supportOrder: SupportOrder): SupportRecord = supportOrder match {
      case (s: SupportHoldOrder) => copy(_holdCount = _holdCount - s)
      case (s: SupportMoveOrder) => copy(_moveCount = _moveCount - s)
    }

    def delSupportTarget(m: MoveOrder): SupportRecord = {
      copy(_moveCount = _moveCount.filterNot { case (s, o) => o == m })
    }

    def supportTarget(supportOrder: SupportOrder): Option[Order] = supportOrder match {
      case (s: SupportHoldOrder) => _holdCount.get(s)
      case (s: SupportMoveOrder) => _moveCount.get(s)
    }

    def supportCount(order: Order, markRecord: MarkRecord): Int = order match {
      case (o: NonMoveOrder) => _holdCount.count { case (k, v) => v == order }
      case (m: MoveOrder) if markRecord.getMark(order).fold(false)(_.isInstanceOf[VoidMark]) => _holdCount.count { case (k, v) => v == order }
      case (m: MoveOrder) if markRecord.getMark(order).fold(false)(_.isInstanceOf[NoConvoy]) => _holdCount.count { case (k, v) => v == order }
      //      case (m: MoveOrder) if markRecord.getMark(order).isDefined => 0
      case (m: MoveOrder) => _moveCount.count { case (k, v) => v == order }
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
      _moveCount.count { case (k, v) => v == moveOrder } - getNoHelpList(moveOrder).size
    }


  }

  case class CombatListRecord(map: Map[Province, Set[Order]] = Map.empty) {

    val provinces: Seq[Province] = map.keys.toSeq

    def orders(province: Province): Seq[Order] = map.get(province).fold(Seq.empty[Order])(_.toSeq)

    def add(province: Province, order: Order): CombatListRecord = copy(map = map.updated(province, map.getOrElse(province, Set.empty) + order))

    def del(o: MoveOrder): CombatListRecord = copy(map = map.updated(o.dst.province, map.getOrElse(o.dst.province, Set.empty) - o))

    def del(province: Province, order: Order): CombatListRecord = copy(map = map.updated(province, map(province) - order))

    def toSeq: Seq[(Province, Set[Order])] = map.toSeq

  }

  case class ConvoyingArmies(_convoyingArmies: Map[ConvoyOrder, MoveOrder] = Map.empty) {
    def toSeq: Seq[(MoveOrder, Set[ConvoyOrder])] = _convoyingArmies.toSeq.groupBy { case (c, m) => m }.map { case (move, seq) => (move, seq.map { case (c, m) => c }.toSet) }.toSeq

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

  case class DislodgedList(orders: Set[Order] = Set.empty) {
    def add(order: Order): DislodgedList = copy(orders = orders + order)

    def del(order: Order): DislodgedList = copy(orders = orders - order)

    def contains(order: Order): Boolean = orders.contains(order)
  }

}

case class OrderResults(results: Seq[OrderResult],
                        supportRecord: SupportRecord,
                        convoyingArmies: ConvoyingArmies,
                        convoySucceeded: ConvoySucceeded,
                        combatListRecord: CombatListRecord,
                        dislodgedList: DislodgedList) {
}

case class OrderState(orders: Seq[Order],
                      worldMap: WorldMap,
                      _markRecord: MarkRecord = MarkRecord(),
                      _supportRecord: SupportRecord = SupportRecord(),
                      _convoyingArmies: ConvoyingArmies = ConvoyingArmies(),
                      _convoySucceeded: ConvoySucceeded = ConvoySucceeded(),
                      _combatListRecord: CombatListRecord = CombatListRecord(),
                      _dislodgedList: DislodgedList = DislodgedList()) {


  def resolve: OrderResults = {
    OrderState.steps(this).toOrderResults
  }

  private def toOrderResults: OrderResults = {
    OrderResults(orders.map(o => getMark(o).fold[OrderResult](o.success)(m => o.failure(m))),
      _supportRecord, _convoyingArmies, _convoySucceeded, _combatListRecord, _dislodgedList)

  }

  def moves: Seq[MoveOrder] = orders.collect { case o: MoveOrder => o }

  def holds: Seq[HoldOrder] = orders.collect { case o: HoldOrder => o }

  def supports: Seq[SupportOrder] = orders.collect { case o: SupportOrder => o }

  def convoys: Seq[ConvoyOrder] = orders.collect { case o: ConvoyOrder => o }

  // mark
  def setMark(order: Order, mark: OrderMark): OrderState = {
    copy(_markRecord = _markRecord.setMark(order, mark))
  }

  def getMark(order: Order): Option[OrderMark] = _markRecord.getMark(order)

  def notMarked(order: Order): Boolean = _markRecord.getMark(order).isEmpty


  def delMark(o: Order): OrderState = {
    copy(_markRecord = _markRecord.delMark(o))
  }

  // support

  def addSupport(o: Order, s: SupportOrder): OrderState = copy(_supportRecord = _supportRecord.addSupport(o, s))

  def addNoHelpList(m: MoveOrder, s: SupportMoveOrder): OrderState = copy(_supportRecord = _supportRecord.addNoHelpList(m, s))

  def delSupport(s: SupportOrder): OrderState = copy(_supportRecord = _supportRecord.delSupport(s))

  def delNoHelpList(sm: SupportMoveOrder): OrderState = copy(_supportRecord = _supportRecord.delNoHelpList(sm))

  def delSupportTarget(m: MoveOrder): OrderState = copy(_supportRecord = _supportRecord.delSupportTarget(m))

  def delNoHelpTarget(moveOrder: MoveOrder): OrderState = copy(_supportRecord = _supportRecord.delNoHelpTarget(moveOrder))

  def supportCount(o: Order): Int = _supportRecord.supportCount(o, _markRecord)

  def supportCountNH(m: MoveOrder): Int = _supportRecord.supportCountNH(m)

  def supportTarget(s: SupportOrder): Option[Order] = _supportRecord.supportTarget(s)

  def uniqueHighestSupportedOrder(province: Province): Option[Order] = {
    val (_, os) = orders.filter {
      case (m: MoveOrder) => m.dst ~~ province
      case (o) => o.src ~~ province
    }.map(o => o -> _supportRecord.supportCount(o, _markRecord)).groupBy {
      case (_, sc) => sc
    }.maxBy {
      case (sc, _) => sc
    }
    if (os.size == 1) Option(os.head._1) else None
  }

  def highestSupportCount(province: Province): Int = {
    _combatListRecord.orders(province).map { o => _supportRecord.supportCount(o, _markRecord) }.reduceOption(_ max _).getOrElse(0)
  }

  // no help list
  // convoyingArmies
  def addConvoy(convoyOrder: ConvoyOrder, moveOrder: MoveOrder): OrderState = {
    copy(_convoyingArmies = _convoyingArmies.addConvoy(convoyOrder, moveOrder))
  }

  def convoyTarget(convoyOrder: ConvoyOrder): Option[MoveOrder] = _convoyingArmies.convoyTarget(convoyOrder)

  def convoyAllFleets: Set[ConvoyOrder] = _convoyingArmies.convoyAllFleets

  def convoyAllTargets: Set[MoveOrder] = _convoyingArmies.convoyAllTargets

  def convoyFleets(moveOrder: MoveOrder): Seq[ConvoyOrder] = _convoyingArmies.convoyFleets(moveOrder)

  def convoyGroups: Seq[(MoveOrder, Set[ConvoyOrder])] = _convoyingArmies.toSeq

  def isConvoyFleet(convoyOrder: ConvoyOrder): Boolean = _convoyingArmies.isConvoyFleet(convoyOrder)

  // convoy success

  def addConvoySucceeded(moveOrder: MoveOrder): OrderState = copy(_convoySucceeded = _convoySucceeded.addConvoySucceeded(moveOrder))

  def isConvoySuccess(moveOrder: MoveOrder): Boolean = _convoySucceeded.isConvoySuccess(moveOrder)

  def addCombatList(province: Province, order: Order): OrderState = copy(_combatListRecord = _combatListRecord.add(province, order))

  def delCombatList(o: MoveOrder): OrderState = copy(_combatListRecord = _combatListRecord.del(o))

  def delCombatList(province: Province, order: Order) = copy(_combatListRecord = _combatListRecord.del(province, order))

  // dislodged list
  def addDislodged(order: Order): OrderState = copy(_dislodgedList = _dislodgedList.add(order))

  def delDislodged(order: Order): OrderState = copy(_dislodgedList = _dislodgedList.del(order))

  def isDislodged(order: Order): Boolean = _dislodgedList.contains(order)
}

trait OrderResult {
  def power: Power = order.power

  def order: OrderBase

  def gameUnit: GameUnit = GameUnit(power, order.unitType)

  def run[T](f: OrderBase => T): Option[T]

  def flatRun[T](f: OrderBase => Option[T]): Option[T]

  def mark: Option[OrderMark]
}

case class SuccessResult(order: OrderBase) extends OrderResult {
  override def run[T](f: (OrderBase) => T): Option[T] = Option(f(order))

  override def flatRun[T](f: (OrderBase) => Option[T]): Option[T] = f(order)

  def mark = None
}

case class FailureResult(order: OrderBase, mark: Option[OrderMark] = None) extends OrderResult {
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

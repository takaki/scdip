package scdip

import scdip.Order._
import scdip.OrderMark._
import scdip.OrderState._
import scdip.UnitType.{Army, Fleet}

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

// ref: http://www.floc.net/dpjudge/?page=Algorithm

object OrderState {
  def steps(orderState: OrderState): OrderState = {
    step10(step6to9(step4to5(step3(step2(step1(orderState))))))
  }

  // Step 1. Mark All Invalid Convoy Orders
  private def step1(orderState: OrderState): OrderState = {
    def step1moves(orderState: OrderState): OrderState = orderState.foldMoves {
      case (os, m) if m.src ~~ m.dst => os.setMark(m, VoidMark("same province"))
      case (os, m) if m.unitType == Army && m.explictConvoy =>
        os.addConvoys(m, os.convoys.filter(c => m.src ~~ c.from && m.dst ~~ c.to))
      case (os, m) if m.unitType == Army =>
        val convoys = os.convoys.filter(c => m.src ~~ c.from && m.dst ~~ c.to && orderState.worldMap.validConvoy(c.from.province, c.to.province, c.src.province))
        // filter related convoy only
        if (m.isNeighbour(os.worldMap)) {
          if (convoys.exists(c => c.power == m.power)) {
            os.addConvoys(m, convoys)
          } else {
            os
          }
        } else {
          os.addConvoys(m, convoys)
        }
      case (os, m) if m.unitType == Fleet && !m.isNeighbour(os.worldMap) => os.setMark(m, VoidMark("fleet can't jump"))
      case (os, _) => os
    }

    def step1convoys(orderState: OrderState): OrderState = {
      (orderState.convoys.toSet -- orderState.convoyAllFleets).foldLeft(orderState) {
        case (os, c) if c.unitType == Army => os.setMark(c, VoidMark("army can't convoy"))
        case (os, c) if c.unitType == Fleet => os.setMark(c, VoidMark("no convoy target"))
        case (os, _) => os
      }
    }

    def checkMoves(orderState: OrderState): OrderState = {
      orderState.convoyGroups.foldLeft(orderState) {
        case (os, (m, _)) if !os.worldMap.canConvoy(m.src.province, m.dst.province) => os.setMark(m, VoidMark("no path")).delConvoy(m)
        case (os, (m, cs)) => if (os.worldMap.canConvoy(m.src.province, m.dst.province, orderState.orders.collect {
          case (o) if o.unitType == Fleet => o.src.province
        }.toSet)) {
          if (os.worldMap.canConvoy(m.src.province, m.dst.province, cs.map(_.src.province))) {
            os
          } else {
            if (m.explictConvoy && m.isNeighbour(os.worldMap)) {
              os.delConvoy(m)
            } else {
              os.setMark(m, NoConvoy("no convoy path")).delConvoy(m)
            }
          }
        } else {
          os.setMark(m, VoidMark("not fleets exist")).delConvoy(m)
        }
      }
    }

    checkMoves(step1convoys(step1moves(orderState)))
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
            case (m: MoveOrder) if os.getMark(m).exists(_.isInstanceOf[VoidMark]) => os.addSupport(m, s)
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

    check1(check0(orderState))
  }

  //  Step 3. Calculate Initial Combat Strengths
  private def step3(orderState: OrderState): OrderState = {
    val os2 = orderState.foldMoves((os, m) => cutSupport(os, m, "step3"), _.filterNot(orderState.isConvoyTarget))
    os2.orders.foldLeft(os2) {
      case (os, m: MoveOrder) if orderState.notMarked(m) => os.addCombatList(m.dst.province, m)
      case (os, m: MoveOrder) => os.addCombatList(m.src.province, m)
      case (os, o) => os.addCombatList(o.src.province, o)
    }
  }

  private def cutSupport(orderState: OrderState, moveOrder: MoveOrder, message: String, after: ((OrderState) => OrderState) = identity, inStep9: Boolean = false): OrderState = {
    def isInvalidSupport(os: OrderState, s: SupportOrder): Boolean = {
      os.getMark(s).exists(m => m.isInstanceOf[CutMark] || m.isInstanceOf[VoidMark])
    }

    orderState.supports.filter(s => moveOrder.dst ~~ s.src)
      .filterNot(s => // condition list of can't cut
        isInvalidSupport(orderState, s) ||
          s.power == moveOrder.power ||
          (orderState.isConvoyTarget(moveOrder) && orderState.supportTarget(s).exists {
            case (c: ConvoyOrder) if !orderState.getMark(c).exists(_.isInstanceOf[VoidMark]) =>
              orderState.convoyFleets(moveOrder).forall(c0 => orderState.isParadox(c, c0))
            case (supportedMove: MoveOrder) =>
              (for {
                c <- orderState.convoys if c.src ~~ supportedMove.dst && !orderState.getMark(c).exists(_.isInstanceOf[VoidMark])
                c0 <- orderState.convoyFleets(moveOrder)
              } yield (c, c0)).forall {
                case (c, c0) => orderState.isParadox(c, c0)
              }
            case _ => false
          }) ||
          (!inStep9 && orderState.supportTarget(s).exists {
            case (m: MoveOrder) => m.dst ~~ moveOrder.src
            case _ => false
          }))
      .foldLeft(orderState) {
        case (os, sh: SupportHoldOrder) => after(os.setMark(sh, CutMark(s"$message; $moveOrder")).delSupport(sh))
        case (os, sm: SupportMoveOrder) => after(os.setMark(sm, CutMark(s"$message; $moveOrder")).delSupport(sm).delNoHelpList(sm))
      }
  }

  // Step 4. Mark Support Cuts Made by Convoyers and Mark Endangered Convoys
  private def step4(orderState: OrderState): OrderState = {

    def impl(orderState: OrderState): OrderState = {
      orderState.convoyAllTargets.foldLeft(orderState) {
        case (os, m) => if (os.notMarked(m)) {
          impl2(cutSupport(os, m, "step4-cut-by-convoyed"), m)
        } else {
          os.setMark(m, ConvoyUnderAttack())
        }
      }

    }

    def impl2(orderState: OrderState, moveOrder: MoveOrder): OrderState = {
      if (orderState.isConvoySuccess(moveOrder)) {
        orderState
      } else {
        step4(orderState.addConvoySucceeded(moveOrder))
      }
    }

    impl(checkDisruption(orderState))
  }

  private def checkDisruption(orderState: OrderState): OrderState = {
    orderState.convoyGroups.foldLeft(orderState) {
      case (os, (m, cs)) =>
        val safe = cs.filter(c => !os.uniqueStrongestOrder(c.src.province).exists(ho => ho.power != c.power))
        if (m.canConvoy(os.worldMap, safe.toSeq)) {
          os
        } else {
          os.setMark(m, ConvoyEndangered())
        }
    }
  }

  // Step 5. Mark Convoy Disruptions And Support Cuts Made by Successful Convoys
  private def step5(orderState: OrderState): OrderState = {
    def impl0(orderState: OrderState) = {
      orderState.convoyAllTargets.foldLeft(orderState) {
        case (os, m) =>
          if (os.getMark(m).exists(_.isInstanceOf[ConvoyEndangered])) {
            impl1(os.setMark(m, NoConvoy("step5-endangered")).delSupportTarget(m).delCombatList(m).addCombatList(m.src.province, m), m)
          } else {
            if (os.getMark(m).exists(_.isInstanceOf[ConvoyUnderAttack])) {
              (impl2(_: OrderState, m)).compose(cutSupport(_: OrderState, m, "step5-1"))(os.delMark(m))
            } else {
              os
            }
          }
      }
    }

    def impl1(orderState: OrderState, m: MoveOrder): OrderState = {
      orderState.convoyFleets(m).foldLeft(orderState) {
        case (os, c) => os.setMark(c, NoConvoy("step5-target-endangered"))
      }
    }

    def impl2(orderState: OrderState, m: MoveOrder): OrderState = {
      if (orderState.isConvoySuccess(m)) {
        orderState
      } else {
        step4to5(orderState.addConvoySucceeded(m))
      }
    }

    impl0(checkDisruption(orderState))
  }

  def step4to5(orderState: OrderState): OrderState = {
    step5(step4(orderState))
  }

  // Step 6. Mark Bounces Caused by Inability to Swap Places
  private def step6(orderState: OrderState): OrderState = {
    (for {
      m <- orderState.moves.filter(m => !orderState.isConvoyTarget(m) && orderState.notMarked(m))
      swapper <- orderState.moves if orderState.notMarked(swapper) && swapper.dst ~~ m.src && swapper.src ~~ m.dst && !orderState.isConvoyTarget(swapper)
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
    orderState.setMark(moveOrder, Bounce(message)).addCombatList(moveOrder.src.province, moveOrder)
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
    val bounced = orderState.combatListSeq.flatMap {
      case (p, cos) => cos.collect {
        case (m: MoveOrder) if orderState.notMarked(m) => m
      }.filter(m =>
        cos.filter(o => o != m).exists {
          case (o: MoveOrder) if orderState.getMark(o).exists(_.isInstanceOf[Bounce]) && o.src ~~ p => 0 >= orderState.supportCount(m)
          case (sh: SupportHoldOrder) if orderState.supportTarget(sh).exists(s => orderState.convoyFleets(m).contains(s)) => orderState.delSupport(sh).uniqueStrongestOrder(sh.targetSrc.province).isDefined // F.18
          case (o) => orderState.supportCount(o) >= orderState.supportCount(m)
        }
      )
    }

    if (bounced.isEmpty) {
      orderState
    } else {
      step6to7(bounced.foldLeft(orderState) {
        case (os, m) => bounce(os, m, "step7")
      })
    }
  }

  // Step 8. Mark Bounces Caused by Inability to Self-Dislodge
  private def step8(orderState: OrderState): OrderState = {
    orderState.combatListSeq.flatMap {
      case (p, cos) =>
        val highest: Int = orderState.highestSupportCount(p)
        val highestOrders = cos.collect {
          case (m: MoveOrder) if orderState.supportCount(m) == highest => m
        }
        if (highestOrders.size == 1) {
          highestOrders.filter(orderState.notMarked)
        } else {
          Seq.empty
        }
    }.foldLeft(orderState) {
      case (os, m) => os.orders.find(h => h.src ~~ m.dst).fold(os) {
        case (o: MoveOrder) if orderState.notMarked(o) => os
        case nm if nm.power == m.power => step6to8(bounce(os, m, "step8 self-attack"))
        case _ => if (os.supportCountNH(m) > os.combatOrders(m.dst.province).collect {
          case o if o != m => os.supportCount(o)
        }.reduceOption(_ max _).getOrElse(0)) {
          os
        } else {
          step6to8(bounce(os, m, "step8-NH"))
        }
      }
    }
  }

  // Step 9. Mark Supports Cut By Dislodgements
  private def step9(orderState: OrderState): OrderState = {
    orderState.foldMoves((os, m) => cutSupport(os, m, "step9", step6to9, inStep9 = true),
      _.filter(o => orderState.notMarked(o)))
  }

  //Step 10. Move Units That Did Not Bounce
  private def step10(orderState: OrderState): OrderState = {
    @scala.annotation.tailrec
    def unbounce(orderState: OrderState, province: Province): OrderState = {
      val orders = orderState.combatOrders(province)
      if (orders.isEmpty) {
        orderState
      } else {
        val (_, maximum) = orders.map(o => (o, orderState.supportCount(o))).groupBy {
          case (_, sc) => sc
        }.maxBy {
          case (sc, _) => sc
        }
        if (maximum.size == 1) {
          val (max, _) = maximum.head
          if (orderState.getMark(max).exists(_.isInstanceOf[Bounce])) {
            val os = orderState.delMark(max)
            if (os.isDislodged(max)) {
              os.delDislodged(max)
            } else {
              unbounce(os.delCombatList(province, max), province)
            }
          } else {
            orderState
          }
        } else {
          orderState
        }
      }
    }

    val dislodgedPairs: Seq[(MoveOrder, Order)] = orderState.moves.filter(orderState.notMarked).flatMap(m => orderState.orders.find {
      case (o: NonMoveOrder) if m.dst ~~ o.src => true
      case (o: MoveOrder) if !orderState.notMarked(o) && m.dst ~~ o.src => true
      case _ => false
    }.map(o => (m, o)))

    def makeDislodged(orderState: OrderState, dp: Seq[(MoveOrder, Order)]): OrderState = {
      dislodgedPairs.foldLeft(orderState) {
        case (os, (m, o: MoveOrder)) if !os.isConvoyTarget(m) && !os.isConvoyTarget(o) && m.src ~~ o.dst =>
          os.delCombatList(o).addDislodged(o, m)
        case (os, (m, o)) => os.addDislodged(o, m)
      }
    }

    dislodgedPairs.foldLeft(makeDislodged(orderState, dislodgedPairs)) {
      case (os, (m, _)) => unbounce(os, m.src.province)
    }
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
      copy(_moveCount = _moveCount.filterNot { case (_, o) => o == m })
    }

    def supportTarget(supportOrder: SupportOrder): Option[Order] = supportOrder match {
      case (s: SupportHoldOrder) => _holdCount.get(s)
      case (s: SupportMoveOrder) => _moveCount.get(s)
    }

    def supportCount(order: Order, markRecord: MarkRecord): Int = order match {
      case (_: NonMoveOrder) => _holdCount.count { case (_, v) => v == order }
      case (_: MoveOrder) if markRecord.getMark(order).exists(_.isInstanceOf[VoidMark]) => _holdCount.count { case (_, v) => v == order }
      case (_: MoveOrder) if markRecord.getMark(order).exists(_.isInstanceOf[NoConvoy]) => _holdCount.count { case (_, v) => v == order }
      case (_: MoveOrder) => _moveCount.count { case (_, v) => v == order }
    }


    def addNoHelpList(moveOrder: MoveOrder, supportMoveOrder: SupportMoveOrder): SupportRecord = {
      copy(_noHelps = _noHelps + (supportMoveOrder -> moveOrder))
    }

    def delNoHelpList(supportMoveOrder: SupportMoveOrder): SupportRecord = {
      copy(_noHelps = _noHelps - supportMoveOrder)
    }

    def delNoHelpTarget(moveOrder: MoveOrder): SupportRecord = {
      copy(_noHelps = _noHelps.filterNot { case (_, o) => o == moveOrder })
    }

    def getNoHelpList(moveOrder: MoveOrder): Seq[SupportMoveOrder] = {
      _noHelps.collect { case (sm, m) if m ~~ moveOrder => sm }.toSeq
    }

    def supportCountNH(moveOrder: MoveOrder): Int = {
      _moveCount.count { case (_, v) => v == moveOrder } - getNoHelpList(moveOrder).size
    }


  }

  case class CombatListRecord(map: Map[Province, Set[Order]] = Map.empty) {

    val provinces: Seq[Province] = map.filter(p => p._2.nonEmpty).keys.toSeq

    def orders(province: Province): Seq[Order] = map.get(province).fold(Seq.empty[Order])(_.toSeq)

    def add(province: Province, order: Order): CombatListRecord = copy(map = map.updated(province, map.getOrElse(province, Set.empty) + order))

    def del(o: MoveOrder): CombatListRecord = copy(map = map.updated(o.dst.province, map.getOrElse(o.dst.province, Set.empty) - o))

    def del(province: Province, order: Order): CombatListRecord = copy(map = map.updated(province, map(province) - order))

    def toSeq: Seq[(Province, Set[Order])] = map.toSeq

  }

  case class ConvoyingArmies(_convoyingArmies: Map[MoveOrder, Set[ConvoyOrder]] = Map.empty) {
    def toSeq: Seq[(MoveOrder, Set[ConvoyOrder])] = _convoyingArmies.toSeq

    def addConvoyMove(moveOrder: MoveOrder): ConvoyingArmies = {
      copy(_convoyingArmies.updated(moveOrder, _convoyingArmies.getOrElse(moveOrder, Set.empty)))
    }


    def addConvoy(moveOrder: MoveOrder, convoyOrder: ConvoyOrder): ConvoyingArmies = {
      copy(_convoyingArmies.updated(moveOrder, _convoyingArmies.getOrElse(moveOrder, Set.empty) + convoyOrder))
    }

    def delConvoy(m: MoveOrder): ConvoyingArmies = copy(_convoyingArmies - m)

    def convoyAllFleets: Set[ConvoyOrder] = _convoyingArmies.values.flatten.toSet

    def convoyAllTargets: Set[MoveOrder] = _convoyingArmies.keys.toSet

    def convoyFleets(moveOrder: MoveOrder): Seq[ConvoyOrder] = _convoyingArmies.getOrElse(moveOrder, Set.empty).toSeq

    def isConvoyFleet(convoyOrder: ConvoyOrder): Boolean = convoyAllFleets.contains(convoyOrder)

    def isConvoyTarget(moveOrder: MoveOrder): Boolean = _convoyingArmies.contains(moveOrder)
  }

  case class ConvoySucceeded(_convoySucceeded: Set[MoveOrder] = Set.empty) {
    def addConvoySucceeded(m: MoveOrder): ConvoySucceeded = copy(_convoySucceeded = _convoySucceeded + m)

    def isConvoySuccess(m: MoveOrder): Boolean = _convoySucceeded.contains(m)
  }

  case class DislodgedList(_dislodged: Map[Order, MoveOrder] = Map.empty) {
    def retreatAreas(worldMap: WorldMap, combatProvinces: Set[Province]): Seq[(UnitPosition, Set[Location])] = _dislodged.toSeq.map {
      case (o, m) => (o.unitPosition, worldMap.neighbours(o.src, combatProvinces + m.src.province))
    }

    def provinces: Seq[Location] = _dislodged.keys.map(_.src).toSeq

    def add(order: Order, moveOrder: MoveOrder): DislodgedList = copy(_dislodged = _dislodged + (order -> moveOrder))

    def del(order: Order): DislodgedList = copy(_dislodged = _dislodged - order)

    def contains(order: Order): Boolean = _dislodged.contains(order)
  }

}

case class OrderResults(results: Seq[OrderResult],
                        supportRecord: SupportRecord,
                        convoyingArmies: ConvoyingArmies,
                        convoySucceeded: ConvoySucceeded,
                        combatListRecord: CombatListRecord,
                        dislodgedList: DislodgedList) {
  def doMove(_unitLocation: UnitLocation): UnitLocation = {
    def afterDislodged(unitLocation: UnitLocation): UnitLocation = {
      dislodgedList.provinces.foldLeft(unitLocation) { (u, l) => u.clear(l) }
    }

    def successResults: Seq[MoveOrder] = {
      results.collect {
        case (r: SuccessResult) => r.order
      }.collect {
        case (m: MoveOrder) => m
      }
    }

    @scala.annotation.tailrec
    def move(unitLocation: UnitLocation, moves: Seq[MoveOrder]): UnitLocation = {
      if (moves.isEmpty) {
        unitLocation
      } else {
        val (s, f) = moves.partition(m => unitLocation.isClear(m.dst))
        if (s.isEmpty && f.nonEmpty) {
          // maybe circular
          f.foldLeft(f.foldLeft(unitLocation) {
            case (u, m) => u.clear(m.src)
          }) {
            case (u, m) => u.updated(UnitPosition(m.dst, m.gameUnit))
          }
        } else {
          move(s.foldLeft(unitLocation) {
            case (u, m) => u.clear(m.src).updated(UnitPosition(m.dst, m.gameUnit))
          }, f)
        }
      }
    }

    move(afterDislodged(_unitLocation), successResults)
  }

  private def retreatAreas(worldMap: WorldMap): Seq[(UnitPosition, Set[Location])] = dislodgedList.retreatAreas(worldMap, combatListRecord.provinces.toSet)

  def retreatArea(worldMap: WorldMap, unitPosition: UnitPosition): Set[Location] = retreatAreas(worldMap).find(_._1 == unitPosition).map(_._2).getOrElse(Set.empty)

  def retreatUnitPositions(worldMap: WorldMap): Seq[UnitPosition] = retreatAreas(worldMap).collect {
    case (o, ls) if ls.nonEmpty => o
  }

  def disbandUnitPosittions(worldMap: WorldMap): Seq[UnitPosition] = retreatAreas(worldMap).collect {
    case (o, ls) if ls.isEmpty => o
  }

}

case class OrderState(orders: Seq[Order],
                      worldMap: WorldMap,
                      _markRecord: MarkRecord = MarkRecord(),
                      _supportRecord: SupportRecord = SupportRecord(),
                      _convoyingArmies: ConvoyingArmies = ConvoyingArmies(),
                      _convoySucceeded: ConvoySucceeded = ConvoySucceeded(),
                      _combatListRecord: CombatListRecord = CombatListRecord(),
                      _dislodgedList: DislodgedList = DislodgedList()) {
  lazy val orderGraph: Graph[Order, DiEdge] = Graph.from(edges = {
    (for {
      x <- orders
      y <- orders
    } yield (x, y)).collect {
      case (c: ConvoyOrder, m: MoveOrder) if c.from ~~ m.src && c.to ~~ m.dst => DiEdge(c, m)
      case (s: SupportMoveOrder, m: MoveOrder) if s.from ~~ m.src && s.to ~~ m.dst => DiEdge(s, m)
      case (s: SupportHoldOrder, nm: NonMoveOrder) if s.targetSrc ~~ nm.src => DiEdge(s, nm)
      case (m: MoveOrder, s: SupportOrder) if m.dst ~~ s.src => DiEdge(m, s)
      case (m: MoveOrder, c: ConvoyOrder) if m.dst ~~ c.src => DiEdge(m, c)
    }
  })

  def isParadox(c: ConvoyOrder, c0: ConvoyOrder): Boolean =
    orderGraph.get(c).findCycle.exists(cycle => cycle.nodes.exists(o => o.value == c0))

  def resolve: OrderResults = {
    OrderState.steps(this).toOrderResults
  }

  private def toOrderResults: OrderResults = {
    OrderResults(orders.map(o => getMark(o).fold[OrderResult](o.success)(m => o.failure(m))),
      _supportRecord, _convoyingArmies, _convoySucceeded, _combatListRecord, _dislodgedList)

  }

  def moves: Seq[MoveOrder] = orders.collect { case o: MoveOrder => o }

  def foldMoves(f: (OrderState, MoveOrder) => OrderState, modifier: (Seq[MoveOrder] => Seq[MoveOrder]) = identity): OrderState = modifier(moves).foldLeft(this)(f)

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

  def uniqueStrongestOrder(province: Province): Option[Order] = {
    val mos: Seq[(Order, Int)] = orders.filter {
      case (m: MoveOrder) => m.dst ~~ province
      case (o) => o.src ~~ province
    }.map(o => (o, _supportRecord.supportCount(o, _markRecord))).groupBy {
      case (_, sc) => sc
    }.maxBy {
      case (sc, _) => sc
    }._2
    if (mos.size == 1) Option(mos.head._1) else None
  }

  def highestSupportCount(province: Province): Int = {
    _combatListRecord.orders(province).map { o => _supportRecord.supportCount(o, _markRecord) }.reduceOption(_ max _).getOrElse(0)
  }

  // no help list

  // convoyingArmies
  def addConvoyMove(moveOrder: MoveOrder): OrderState = {
    copy(_convoyingArmies = _convoyingArmies.addConvoyMove(moveOrder))
  }

  def addConvoy(convoyOrder: ConvoyOrder, moveOrder: MoveOrder): OrderState = {
    copy(_convoyingArmies = _convoyingArmies.addConvoy(moveOrder, convoyOrder))
  }

  def addConvoys(m: MoveOrder, convoys: Seq[ConvoyOrder]): OrderState = {
    convoys.foldLeft(addConvoyMove(m)) {
      case (os, (c)) => os.addConvoy(c, m)
    }
  }

  def delConvoy(m: MoveOrder): OrderState = {
    copy(_convoyingArmies = _convoyingArmies.delConvoy(m))
  }

  def convoyAllFleets: Set[ConvoyOrder] = _convoyingArmies.convoyAllFleets

  def convoyAllTargets: Set[MoveOrder] = _convoyingArmies.convoyAllTargets

  def convoyFleets(moveOrder: MoveOrder): Seq[ConvoyOrder] = _convoyingArmies.convoyFleets(moveOrder)

  def convoyGroups: Seq[(MoveOrder, Set[ConvoyOrder])] = _convoyingArmies.toSeq

  def isConvoyFleet(convoyOrder: ConvoyOrder): Boolean = _convoyingArmies.isConvoyFleet(convoyOrder)

  def isConvoyTarget(moveOrder: MoveOrder): Boolean = _convoyingArmies.isConvoyTarget(moveOrder)

  // convoy success

  def addConvoySucceeded(moveOrder: MoveOrder): OrderState = copy(_convoySucceeded = _convoySucceeded.addConvoySucceeded(moveOrder))

  def isConvoySuccess(moveOrder: MoveOrder): Boolean = _convoySucceeded.isConvoySuccess(moveOrder)

  def addCombatList(province: Province, order: Order): OrderState = copy(_combatListRecord = _combatListRecord.add(province, order))

  def delCombatList(o: MoveOrder): OrderState = copy(_combatListRecord = _combatListRecord.del(o))

  def delCombatList(province: Province, order: Order): OrderState = copy(_combatListRecord = _combatListRecord.del(province, order))

  // combat list
  def combatListAdd(province: Province, order: Order): OrderState = {
    copy(_combatListRecord = _combatListRecord.add(order.src.province, order))
  }

  def combatListSeq: Seq[(Province, Set[Order])] = _combatListRecord.toSeq

  def combatOrders(province: Province): Seq[Order] = _combatListRecord.orders(province)

  // dislodged list
  def addDislodged(order: Order, moveOrder: MoveOrder): OrderState = copy(_dislodgedList = _dislodgedList.add(order, moveOrder))

  def delDislodged(order: Order): OrderState = copy(_dislodgedList = _dislodgedList.del(order))

  def isDislodged(order: Order): Boolean = _dislodgedList.contains(order)
}

trait OrderResult {
  def power: Power = order.power

  def order: Order

  def gameUnit: GameUnit = GameUnit(power, order.unitType)

  def run[T](f: Order => T): Option[T]

  def flatRun[T](f: Order => Option[T]): Option[T]

  def mark: Option[OrderMark]
}

case class SuccessResult(order: Order) extends OrderResult {
  override def run[T](f: (Order) => T): Option[T] = Option(f(order))

  override def flatRun[T](f: (Order) => Option[T]): Option[T] = f(order)

  def mark = None
}

case class FailureResult(order: Order, mark: Option[OrderMark] = None) extends OrderResult {
  override def run[T](f: (Order) => T): Option[T] = None

  override def flatRun[T](f: (Order) => Option[T]): Option[T] = None
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

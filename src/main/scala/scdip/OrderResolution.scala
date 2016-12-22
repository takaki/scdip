package scdip

import scdip.Order._


trait OrderAdjudicator {
  def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState
}

case object AdjudicatorStep1 extends OrderAdjudicator {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    val moves = orderState.moves.map(move =>
      if (move.action.unitType == UnitType.Army &&
        move.requireConvoy(worldMap) &&
        !worldMap.canConvoy(move.src.province, move.action.dst.province,
          restrict = orderState.convoys.filter(c => c.action.convoyMove == move.action)
            .map(c => c.src.province).toSet)) {
        move.copy(mark = Option("no convoy"))
      } else {
        move
      })
    val convoys = orderState.convoys.map(convoy =>
      if (convoy.action.unitType == UnitType.Fleet && orderState.moves.exists(m => convoy.action.convoyMove == m.action)) {
        convoy
      } else {
        convoy.copy(mark = Option("void"))
      }
    )
    orderState.copy(moves = moves, convoys = convoys)
  }
}

case object AdjudicatorStep2 extends OrderAdjudicator {
  override def evaluate(worldMap: WorldMap, orderState: OrderState): OrderState = {
    val moves = orderState.moves.map(move =>
      if (move.action.unitType == UnitType.Fleet) {
        if (worldMap.isNeighbour(move.action.src, move.action.dst)) {
          move
        } else {
          move.copy(mark = Option("void"))
        }
      } else {
        move
      })
    val suportHolds = orderState.supportHolds.map(s =>
      s
    )
    orderState.copy(moves = moves)
  }
}

case class OrderState(holds: Seq[HoldOrder] = Seq.empty,
                      moves: Seq[MoveOrder] = Seq.empty,
                      supportHolds: Seq[SupportHoldOrder] = Seq.empty,
                      supportMoves: Seq[SupportMoveOrder] = Seq.empty,
                      convoys: Seq[ConvoyOrder] = Seq.empty
                     ) {
  def results: Seq[OrderResult] = {
    (holds ++ moves ++ supportHolds ++ supportMoves ++ convoys).map(_.result)
  }
}

object OrderState {
  def fromSeq(orders: Seq[Order]): OrderState = {
    val orderMap = orders.groupBy(_.getClass)
    OrderState(holds = orderMap.getOrElse(classOf[HoldOrder], Seq.empty).map(_.asInstanceOf[HoldOrder]),
      moves = orderMap.getOrElse(classOf[MoveOrder], Seq.empty).map(_.asInstanceOf[MoveOrder]),
      supportHolds = orderMap.getOrElse(classOf[SupportHoldOrder], Seq.empty).map(_.asInstanceOf[SupportHoldOrder]),
      supportMoves = orderMap.getOrElse(classOf[SupportMoveOrder], Seq.empty).map(_.asInstanceOf[SupportMoveOrder]),
      convoys = orderMap.getOrElse(classOf[ConvoyOrder], Seq.empty).map(_.asInstanceOf[ConvoyOrder])
    )
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


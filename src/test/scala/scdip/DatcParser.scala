package scdip

import scdip.Action._
import scdip.Order._
import scdip.PhaseType.{Adjustment, Movement, Retreat}
import scdip.Season.{Fall, Spring}
import scdip.UnitType.{Army, Fleet}

import scala.util.matching.Regex
import scala.util.parsing.combinator._


case class DatcParser(variant: Variant) extends OrderParser with PhaseParser {

  override protected val whiteSpace: Regex = "( |\\t|#.*)+".r

  def parse(input: String): Either[String, List[Datc]] = parseAll(datc, input) match {
    case Success(data, next) => Right(data)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }

  def datc: Parser[List[Datc]] = rep(LF) ~> header ~> rep(LF) ~> testcases <~ rep(LF)

  def header: Parser[String] = "VARIANT_ALL Standard" <~ LF

  def testcases: Parser[List[Datc]] = repsep(testcase, rep(LF))

  def LF = "\n"

  def testcase: Parser[Datc] = casename ~
    opt(prestateSetphase) ~ opt(prestateSupplycenterOwners) ~ prestate ~ opt(prestateDislodged) ~ opt(prestateResults) ~
    orders ~ (poststateBlock | poststateSame) <~ "END" <~ opt(LF) ^^ {
    case (t ~
      prePhase ~ owners ~ preSt ~ preD ~ preR ~
      os ~ postSt) =>
      val phase = prePhase.getOrElse(Phase(1901, Spring, Movement))
      val psomap = owners.toList.flatten.foldLeft(Map.empty[Province, Power])((m, us) => m.updated(us.location.province, us.gameUnit.power))
      val preDis = preD.toList.flatten
      val preRes = preR.toList.flatten
      if (postSt._1.isEmpty && postSt._2.isEmpty) {
        Datc(variant, t, phase, psomap, preSt, preDis, preRes, os, preSt, Seq.empty)
      } else {
        Datc(variant, t, phase, psomap, preSt, preDis, preRes, os, postSt._1, postSt._2)
      }
  }

  def prestateDislodged: Parser[List[UnitState]] = "PRESTATE_DISLODGED" ~> LF ~> rep(state)

  def prestateResults: Parser[List[OrderResult]] = "PRESTATE_RESULTS" ~> LF ~> rep(result)

  def result: Parser[OrderResult] = flag ~ order <~ rep1(LF) ^^ { case (f ~ o) => f(o.power, o.action) }

  def flag: Parser[(Power, Action) => OrderResult] = (success | failure) <~ ":"

  def success: Parser[SuccessResult.type] = "SUCCESS" ^^ { _ => SuccessResult }

  def failure: Parser[FailureResult.type] = "FAILURE" ^^ { _ => FailureResult }

  def casename: Parser[String] = "CASE" ~> ".+".r <~ LF ^^ { result => result }

  def prestateSupplycenterOwners: Parser[List[UnitState]] = "PRESTATE_SUPPLYCENTER_OWNERS" ~> LF ~> rep(state)

  def prestateSetphase: Parser[Phase] = "PRESTATE_SETPHASE" ~> phase <~ LF

  def prestate: Parser[List[UnitState]] = "PRESTATE" ~> LF ~> rep(state)

  def state: Parser[UnitState] = power ~ unittype ~ (location <~ rep1(LF)) ^^ { case (p ~ u ~ l) => UnitState(l.setCoast(u), GameUnit(p, u)) }

  def orders: Parser[List[Order]] = "ORDERS" ~> rep1(LF) ~> rep(order <~ LF)

  def poststateBlock: Parser[(List[UnitState], List[UnitState])] = poststate ~ opt(poststateDislodged) ^^ { case (ps ~ pd) => (ps, pd.toList.flatten) }

  def poststate: Parser[List[UnitState]] = "POSTSTATE" ~> rep1(LF) ~> rep(state) <~ opt(LF)

  def poststateDislodged: Parser[List[UnitState]] = "POSTSTATE_DISLODGED" ~> rep1(LF) ~> rep(state) <~ rep(LF)

  def poststateSame: Parser[(List[UnitState], List[UnitState])] = "POSTSTATE_SAME" <~ LF ^^ { result => (List.empty[UnitState], List.empty[UnitState]) }
}

trait OrderParser extends UnitTypeParser with RegexParsers {
  def variant: Variant

  val worldMap: WorldMap = variant.worldMap

  def order: Parser[Order] = holdOrder | moveOrder | supportMoveOrder | supportHoldOrder | convoyOrder | buildOrder | removeOrder

  def holdOrder: Parser[HoldOrder] = power ~ hold ^^ { case (p ~ a) => HoldOrder(p, a) }

  def moveOrder: Parser[MoveOrder] = power ~ move ^^ { case (p ~ a) => MoveOrder(p, a) }

  def supportHoldOrder: Parser[SupportHoldOrder] = power ~ supportHold ^^ { case (p ~ a) => SupportHoldOrder(p, a) }

  def supportMoveOrder: Parser[SupportMoveOrder] = power ~ supportMove ^^ { case (p ~ a) => SupportMoveOrder(p, a) }

  def convoyOrder: Parser[ConvoyOrder] = power ~ convoy ^^ { case (p ~ a) => ConvoyOrder(p, a) }

  def buildOrder: Parser[BuildOrder] = power ~ build ^^ { case (p ~ a) => BuildOrder(p, a) }

  def removeOrder(): Parser[RemoveOrder] = power ~ remove ^^ { case (p ~ a) => RemoveOrder(p, a) }

  def power: Parser[Power] = "[A-Z][a-z]+".r <~ ":" ^^ { result => variant.power(result) }

  def location: Parser[Location] = province ~ opt(coast) ^^ { case (p ~ c) => Location(p, c.getOrElse(Coast.Undefined)) }

  def province: Parser[Province] = "[A-Za-z]+".r ^^ { result => worldMap.province(result) }

  def coast: Parser[Coast] = (("/" ~> "[a-z]+".r) | ("(" ~> "[a-z]+".r <~ ")")) ^^ { result => Coast.parse(result) }

  def hold: Parser[HoldAction] = unittype ~ location <~ ("(?i)HOLD".r | "H") ^^ { case (t ~ s) => HoldAction(t, s.setCoast(t)) }

  // TODO: via convoy?
  def move: Parser[MoveAction] = unittype ~ (location <~ "-") ~ location ~ opt(("via" | "by") ~> "(?i)convoy".r) ^^ {
    case (t ~ s ~ d ~ c) => MoveAction(t, s.setCoast(t), d.setCoast(t), c.isDefined || t.viaConvoy(worldMap, s,d))
  }

  def support: Parser[String] = "(?i)SUPPORTS".r | "S"

  def supportHold: Parser[SupportHoldAction] = unittype ~ (location <~ support) ~ unittype ~ location ^^ {
    case (t ~ s ~ ht ~ hs) => SupportHoldAction(t, s.setCoast(t), HoldAction(ht, hs.setCoast(ht)))
  }

  def supportMove: Parser[SupportMoveAction] = unittype ~ (location <~ support) ~ move ^^ { case (t ~ s ~ m) => SupportMoveAction(t, s.setCoast(t), m) }

  def convoy: Parser[ConvoyAction] = unittype ~ (location <~ ("(?i)convoys".r | "C" | "c")) ~ move ^^ { case (t ~ s ~ m) => ConvoyAction(t, s.setCoast(t), m) }

  def build: Parser[BuildAction] = ("Build" ~> unittype) ~ location ^^ { case (ut ~ l) => BuildAction(ut, l) }

  def remove: Parser[RemoveAction] = ("Remove" ~> unittype) ~ location ^^ { case (ut ~ l) => RemoveAction(ut, l) } // TODO: Army??
}

trait UnitTypeParser extends RegexParsers {

  def unittype: Parser[UnitType] = army | fleet

  def army: Parser[Army.type] = ("(?i)army" | "A" | "a") ^^ { _ => UnitType.Army }

  def fleet: Parser[Fleet.type] = ("(?i)fleet" | "F" | "f") ^^ { _ => UnitType.Fleet }
}



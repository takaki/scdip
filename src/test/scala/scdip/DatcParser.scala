package scdip

import scdip.Order._
import scdip.PhaseType.Movement
import scdip.Season.Spring
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
      val psomap = owners.toList.flatten.foldLeft(Map.empty[Province, Power])((m, up) => m.updated(up.location.province, up.power))
      val preDis = preD.toList.flatten
      val preRes = preR.toList.flatten
      if (postSt._1.isEmpty && postSt._2.isEmpty) {
        Datc(variant, t, phase, psomap, preSt, preDis, preRes, os, preSt, Seq.empty)
      } else {
        Datc(variant, t, phase, psomap, preSt, preDis, preRes, os, postSt._1, postSt._2)
      }
  }

  def prestateDislodged: Parser[List[UnitPosition]] = "PRESTATE_DISLODGED" ~> LF ~> rep(state)

  def prestateResults: Parser[List[OrderResult]] = "PRESTATE_RESULTS" ~> LF ~> rep(result)

  def result: Parser[OrderResult] = flag ~ order <~ rep1(LF) ^^ { case (f ~ o) => f(o) }

  def flag: Parser[(Order) => OrderResult] = (success | failure) <~ ":"

  def success: Parser[SuccessResult.type] = "SUCCESS" ^^ { _ => SuccessResult }

  def failure: Parser[(Order) => FailureResult] = "FAILURE" ^^ { _ => FailureResult(_: Order, None) }

  def casename: Parser[String] = "CASE" ~> ".+".r <~ LF ^^ { result => result }

  def prestateSupplycenterOwners: Parser[List[UnitPosition]] = "PRESTATE_SUPPLYCENTER_OWNERS" ~> LF ~> rep(state)

  def prestateSetphase: Parser[Phase] = "PRESTATE_SETPHASE" ~> phase <~ LF

  def prestate: Parser[List[UnitPosition]] = "PRESTATE" ~> LF ~> rep(state)

  def state: Parser[UnitPosition] = power ~ unittype ~ (location <~ rep1(LF)) ^^ { case (p ~ u ~ l) => UnitPosition(p, u, l.setCoast(u)) }

  def orders: Parser[List[Order]] = "ORDERS" ~> rep1(LF) ~> rep(order <~ LF)

  def poststateBlock: Parser[(List[UnitPosition], List[UnitPosition])] = poststate ~ opt(poststateDislodged) ^^ { case (ps ~ pd) => (ps, pd.toList.flatten) }

  def poststate: Parser[List[UnitPosition]] = "POSTSTATE" ~> rep1(LF) ~> rep(state) <~ opt(LF)

  def poststateDislodged: Parser[List[UnitPosition]] = "POSTSTATE_DISLODGED" ~> rep1(LF) ~> rep(state) <~ rep(LF)

  def poststateSame: Parser[(List[UnitPosition], List[UnitPosition])] = "POSTSTATE_SAME" <~ LF ^^ { result => (List.empty[UnitPosition], List.empty[UnitPosition]) }
}

trait OrderParser extends UnitTypeParser with RegexParsers {
  def variant: Variant

  val worldMap: WorldMap = variant.worldMap

  def order: Parser[Order] = holdOrder | moveOrder | supportMoveOrder | supportHoldOrder | convoyOrder | buildOrder | removeOrder

  def unitPosition: Parser[UnitPosition] = power ~ unittype ~ location ^^ { case (p ~ u ~ l) => UnitPosition(p, u, l.setCoast(u)) }

  def holdOrder: Parser[HoldOrder] = unitPosition <~ ("(?i)HOLD".r | "H") ^^ { case (u) => HoldOrder(u) }

  def moveOrder: Parser[MoveOrder] = (unitPosition <~ "-") ~ location ~ opt(("via" | "by") ~> "(?i)convoy".r) ^^ {
    case (up ~ d ~ c) => MoveOrder(up, d.setDstCoast(up.unitType, up.location, worldMap), c.fold(false)(_ => true))
  }

  def support: Parser[String] = "(?i)SUPPORTS".r | "S"

  def supportHoldOrder: Parser[SupportHoldOrder] = (unitPosition <~ support) ~ unittype ~ location ^^ {
    case (up ~ ht ~ hs) => SupportHoldOrder(up, ht, hs.setCoast(ht))
  }

  def supportMoveOrder: Parser[SupportMoveOrder] = (unitPosition <~ support) ~ unittype ~ (location <~ "-") ~ location ^^ {
    case (up ~ u ~ f ~ to) => SupportMoveOrder(up, u, f, to)
  }

  def convoyOrder: Parser[ConvoyOrder] = (unitPosition <~ ("(?i)convoys".r | "C" | "c")) ~ unittype ~ (location <~ "-") ~ location ^^ {
    case (up ~ u ~ f ~ to) => ConvoyOrder(up, u, f, to)
  }

  def buildOrder: Parser[BuildOrder] = power ~ ("Build" ~> unittype) ~ location ^^ { case (p ~ ut ~ l) => BuildOrder(UnitPosition(p, ut, l.setCoast(ut))) }

  def removeOrder(): Parser[RemoveOrder] = power ~ ("Remove" ~> unittype) ~ location ^^ { case (p ~ ut ~ l) => RemoveOrder(UnitPosition(p, ut, l.setCoast(ut))) }

  def power: Parser[Power] = "[A-Z][a-z]+".r <~ ":" ^^ { result => variant.power(result) }

  def location: Parser[Location] = province ~ opt(coast) ^^ { case (p ~ c) => Location(p, c.flatten) }

  def province: Parser[Province] = "[A-Za-z]+".r ^^ { result => worldMap.province(result) }

  def coast: Parser[Option[Coast]] = (("/" ~> "[a-z]+".r) | ("(" ~> "[a-z]+".r <~ ")")) ^^ { result => Coast.parse(result) }


}

trait UnitTypeParser extends RegexParsers {

  def unittype: Parser[UnitType] = army | fleet

  def army: Parser[Army.type] = ("(?i)army" | "A" | "a") ^^ { _ => UnitType.Army }

  def fleet: Parser[Fleet.type] = ("(?i)fleet" | "F" | "f") ^^ { _ => UnitType.Fleet }
}



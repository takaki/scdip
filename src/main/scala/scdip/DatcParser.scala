package scdip

import scdip.Action._
import scdip.Order._
import scdip.UnitType.{Army, Fleet}

import scala.util.matching.Regex
import scala.util.parsing.combinator._


case object DatcParser extends RegexParsers {
  def apply(input: String): Either[String, Any] = parseAll(testcases, input) match {
    case Success(data, next) => Right(data)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }

  def testcases = ???

  def commenct: Regex = """^#.*""".r

  def variant = "^VARIANT_ALL Standard"

}

case class OrderParser(variant: Variant) extends RegexParsers {
  private val worldMap = variant.worldMap

  def apply(input: String): Either[String, Order] = parseAll(order, input) match {
    case Success(data, next) => Right(data)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }

  def order: Parser[Order with Product with Serializable] = holdOrder | moveOrder | supportHoldOrder | supportMoveOrder | convoyOrder

  def holdOrder: Parser[HoldOrder] = (power <~ ":") ~ hold ^^ { case (p ~ a) => HoldOrder(p, a) }

  def moveOrder: Parser[MoveOrder] = (power <~ ":") ~ move ^^ { case (p ~ a) => MoveOrder(p, a) }

  def supportHoldOrder: Parser[SupportHoldOrder] = (power <~ ":") ~ supportHold ^^ { case (p ~ a) => SupportHoldOrder(p, a) }

  def supportMoveOrder: Parser[SupportMoveOrder] = (power <~ ":") ~ supportMove ^^ { case (p ~ a) => SupportMoveOrder(p, a) }

  def convoyOrder: Parser[ConvoyOrder] = (power <~ ":") ~ convoy ^^ { case (p ~ a) => ConvoyOrder(p, a) }

  def power: Parser[Power] = "[A-Za-z]+".r ^^ { result => variant.power(result).get }

  def unittype: Parser[UnitType] = army | fleet

  def army: Parser[Army.type] = "A" ^^ { _ => UnitType.Army }

  def fleet: Parser[Fleet.type] = "F" ^^ { _ => UnitType.Fleet }

  def location: Parser[Location] = province ~ opt(coast) ^^ { case (p ~ c) => Location(p, c.getOrElse(Coast.Undefined)) }

  def province: Parser[Province] = "[A-Za-z]+".r ^^ { result => worldMap.province(result) }

  def coast: Parser[Coast] = "/" ~> "[a-z]+".r ^^ { result => Coast.parse(result) }

  def hold: Parser[HoldAction] = unittype ~ location <~ "H" ^^ { case (t ~ s) => HoldAction(t, s.setCoast(t)) }

  def move: Parser[MoveAction] = unittype ~ (location <~ "-") ~ location ^^ { case (t ~ s ~ d) => MoveAction(t, s.setCoast(t), d.setCoast(t)) }

  def supportHold: Parser[SupportHoldAction] = unittype ~ (location <~ "S") ~ unittype ~ location ^^ {
    case (t ~ s ~ ht ~ hs) => SupportHoldAction(t, s.setCoast(t), HoldAction(ht, hs.setCoast(ht)))
  }

  def supportMove: Parser[SupportMoveAction] = unittype ~ (location <~ "S") ~ move ^^ { case (t ~ s ~ m) => SupportMoveAction(t, s.setCoast(t), m) }

  def convoy: Parser[ConvoyAction] = unittype ~ (location <~ "[Cc]onvoys".r) ~ move ^^ { case (t ~ s ~ m) => ConvoyAction(t, s.setCoast(t), m) }

}
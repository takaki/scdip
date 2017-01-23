package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scdip.Order.MoveOrder
import scdip.OrderMark.VoidMark

import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class OrderResolutionSpecs extends Specification {
  val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
  val variant: Variant = variants.variant("Standard [No Units]").get
  val parser = new OrderParser {
    override def variant: Variant = variants.variant("Standard [No Units]").get

    def apply(input: String): Order = parseAll(order, input) match {
      case Success(data, next) => data
      case NoSuccess(errorMessage, next) => throw new RuntimeException(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
    }
  }
  val iniState = variant.movementState

  def parseOrders(input: String): OrderState = OrderState(input.lines.map(parser(_)).toList, variant.worldMap)

  "CASE 6.A.5.old (Nov-24-2001 DATC) same place" >> {
    val os = parseOrders(
      """England: F london-Yorkshire
        |England: F nth convoys A Yorkshire - yorkshire
        |England: A Yorkshire-Yorkshire
        |England: A liverpool supports A Yorkshire-Yorkshire""".stripMargin)
    val os0 = os.resolve
    os0.markRecord.getMark(os0.orders(2)) must beSome(VoidMark("same place"))
  }

  "CASE 6.A.5 (Move to own sector with convoy)" >> {
    val os = parseOrders(
      """	England: F nth convoys A Yorkshire - yorkshire
        |	England: A Yorkshire-Yorkshire
        |	England: A liverpool supports A Yorkshire-Yorkshire
        |	Germany: F london-Yorkshire
        |	Germany: A Wales SUPPORTS F london-yorkshire
        |""".stripMargin)
    val os0 = os.resolve
    os0.markRecord.getMark(os0.orders(0)) must beSome
    os0.markRecord.getMark(os0.orders(1)) must beSome(VoidMark("same place"))
    os0.markRecord.getMark(os0.orders(2)) must beNone
    os0.markRecord.getMark(os0.orders(3)) must beNone
    os0.markRecord.getMark(os0.orders(4)) must beNone
    os0.results.collect { case (s: SuccessResult) => s } must have size 3
  }

  "CASE 6.A.10.old (Nov-24-2001 DATC)" >> {
    val os = parseOrders(
      """	Austria: A budapest SUPPORTS F trieste-venice
        |	Austria: F trieste-venice
        |	Italy: A venice HOLD
        |""".stripMargin).resolve
    os.getMark(os.orders(0)) must beSome
    os.supportRecord.supportCount(os.orders(1)) === 0
    os.getMark(os.orders(1)) must beSome
    os.getMark(os.orders(2)) must beNone
  }

  "fail with no path" >> {
    val os = parseOrders("England: A lon - ukr")
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders.head) must beSome
  }
  "fail with no convoy fleets" >> {
    val os = parseOrders("England: A lon - nwy")
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders.head) must beSome
  }
  "correct convoy" >> {
    val os = parseOrders(
      """England: A lon - nwy
        |England: F nth convoys A lon - nwy""".stripMargin)
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders.head) must beNone
    os0.markRecord.getMark(os.orders(1)) must beNone
    os0.convoyingArmies.convoyAllFleets must have size 1
  }
  "void convoy" >> {
    val os = parseOrders("England: F nth C A lon - nwy")
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders.head) must beSome
  }

  "support hold" >> {
    val os = parseOrders(
      """England: A yor H
        |England: A lon S A yor""".stripMargin)
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders(1)) must beNone
    os0.supportRecord.supportCount(os.orders(0)) === 1

  }
  "support hold: fail" >> {
    val os = parseOrders("England: A lon S A yor")
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders(0)) must beSome
    os0.supportRecord.supportCount(os.orders(0)) === 0
  }
  "support move" >> {
    val os = parseOrders(
      """England: A yor - wal
        |England: A lon S A yor - wal""".stripMargin)
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders(1)) must beNone
    os0.supportRecord.supportCount(os.orders(0)) === 1

  }
  "self attack no help list" >> {
    val os = parseOrders(
      """England: A lon - yor
        |England: A yor H
        |England: A wal S A lon - yor""".stripMargin)
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders(1)) must beNone
    //        os0.supportCount(os.orders(0)) === 1
    //        os0.getNoHelpList(os.orders(0).asInstanceOf[MoveOrder]) must have size 1
  }
  "support cut" >> {
    val os = parseOrders(
      """England: A lon S A yor
        |England: A yor H
        |France: A wal - lon""".stripMargin)
    val os0 = os.resolve
    os0.markRecord.getMark(os.orders(0)) must beSome
    os0.markRecord.getMark(os.orders(1)) must beNone
    os0.supportRecord.supportCount(os.orders(1)) === 0
  }
}

package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scdip.Order.MoveOrder

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

  def parseOrders(input: String): OrderState = OrderState(input.lines.map(parser(_)).toList, variant.worldMap)

  "Order Evaluation Steps" >> {
    def adjudicator(os: OrderState) = os.resolve

    "step 1" >> {
      "fail with no path" >> {
        val os = parseOrders("England: A lon - ukr")
        val os0 = adjudicator(os)
        os0.getMark(os.orders.head) must beSome
      }
      "fail with no convoy fleets" >> {
        val os = parseOrders("England: A lon - nwy")
        val os0 = adjudicator(os)
        os0.getMark(os.orders.head) must beSome
      }
      "correct convoy" >> {
        val os = parseOrders(
          """England: A lon - nwy
            |England: F nth convoys A lon - nwy""".stripMargin)
        val os0 = adjudicator(os)
        os0.getMark(os.orders.head) must beNone
        os0.getMark(os.orders(1)) must beNone
        os0._convoyingArmies must have size (1)
      }
      "void convoy" >> {
        val os = parseOrders("England: F nth C A lon - nwy")
        val os0 = adjudicator(os)
        os0.getMark(os.orders.head) must beSome
      }
    }
    "step 2" >> {

      "support hold" >> {
        val os = parseOrders(
          """England: A yor H
            |England: A lon S A yor""".stripMargin)
        val os0 = adjudicator(os)
        os0.getMark(os.orders(1)) must beNone
        os0.supportRecord.supportCount(os.orders(0)) === 1

      }
      "support hold: fail" >> {
        val os = parseOrders("England: A lon S A yor")
        val os0 = adjudicator(os)
        os0.getMark(os.orders(0)) must beSome
        os0.supportRecord.supportCount(os.orders(0)) === 0
      }
      "support move" >> {
        val os = parseOrders(
          """England: A yor - wal
            |England: A lon S A yor - wal""".stripMargin)
        val os0 = adjudicator(os)
        os0.getMark(os.orders(1)) must beNone
        os0.supportRecord.supportCount(os.orders(0)) === 1

      }
      "self attack no help list" >> {
        val os = parseOrders(
          """England: A lon - yor
            |England: A yor H
            |England: A wal S A lon - yor""".stripMargin)
        val os0 = adjudicator(os)
        os0.getMark(os.orders(1)) must beNone
//        os0.supportCount(os.orders(0)) === 1
//        os0.getNoHelpList(os.orders(0).asInstanceOf[MoveOrder]) must have size 1
      }
    }
    "step 3" >> {
      "support cut" >> {
        val os = parseOrders(
          """England: A lon S A yor
            |England: A yor H
            |France: A wal - lon""".stripMargin)
        val os0 = adjudicator(os)
        os0.getMark(os.orders(0)) must beSome
        os0.getMark(os.orders(1)) must beNone
        os0.supportRecord.supportCount(os.orders(1)) === 0
      }
    }
  }
}

package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scdip.Order.{ConvoyOrder, MoveOrder}
import scdip.OrderMark.{NoConvoy, VoidMark}

import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class OrderSpecs extends Specification {
  val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
  val variant = variants.variant("Standard [No Units]").get
  val worldMap = variant.worldMap
  val parser = new OrderParser {
    override def variant: Variant = variants.variant("Standard [No Units]").get

    def apply(input: String): Either[String, Order] = parseAll(order, input) match {
      case Success(data, next) => Right(data)
      case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
    }
  }

  def parseOrders(lines: Seq[String]): OrderState = {
    OrderState(lines.map(parser(_).right.get))
  }

  "Order Evaluation Steps" >> {
    "Adjudicator 1" >> {
      "fail with no path" >> {
        val os = parseOrders(Seq("England: A lon - ukr"))
        val os0 = AdjudicatorStep1.evaluate(worldMap = worldMap, os)
        os0.getMark(os.orders.head) must beSome(NoConvoy())
      }
      "fail with no convoy fleets" >> {
        val os = parseOrders(Seq("England: A lon - nwy"))
        val os0 = AdjudicatorStep1.evaluate(worldMap = worldMap, os)
        os0.getMark(os.orders.head) must beSome(NoConvoy())
      }
      "correct convoy" >> {
        val os = parseOrders(Seq("England: A lon - nwy", "England: F nth convoys A lon - nwy"))
        val os0 = AdjudicatorStep1.evaluate(worldMap = worldMap, os)
        os0.getMark(os.orders.head) must  beNone
        os0.getMark(os.orders(1)) must beNone
      }
      "void convoy" >> {
        val os = parseOrders(Seq("England: F nth convoys A lon - nwy"))
        val os0 = AdjudicatorStep1.evaluate(worldMap = worldMap, os)
        os0.getMark(os.orders.head) must beSome(VoidMark())
      }
    }
    "Adjudicator 2" >> {
      def adjudicator(os: OrderState) = Seq(AdjudicatorStep1, AdjudicatorStep2).foldLeft(os)((os, oes) => oes.evaluate(variant.worldMap, os))

      "support hold" >> {
        val os = parseOrders(Seq("England: A yor H", "England: A lon S A yor"))
        val os0 = adjudicator(os)
        os0.getMark(os.orders(1)) must beNone
        os0.supportCount must havePair(os.orders(0).action -> 1)

      }
      "support hold: fail" >> {
        val os = parseOrders(Seq("England: A lon S A yor"))
        val os0 = adjudicator(os)
        println(os0)
        os0.getMark(os.orders(0)) must beSome
        os0.supportCount must beEmpty
      }

    }
  }
}

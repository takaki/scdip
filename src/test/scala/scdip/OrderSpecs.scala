package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scdip.Order.{ConvoyOrder, MoveOrder}

import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class OrderSpecs extends Specification {
  val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
  val variant = variants.variant("Standard [No Units]").get
  val worldMap = variant.worldMap
  val parser = OrderParser(variant)

  def parseOrders(lines: Seq[String]): OrderState = {
    val orders = lines.map(parser(_).right.get).groupBy(_.getClass)
    OrderState(moves = orders.getOrElse(classOf[MoveOrder], Seq.empty).map(_.asInstanceOf[MoveOrder]),
      convoys = orders.getOrElse(classOf[ConvoyOrder], Seq.empty).map(_.asInstanceOf[ConvoyOrder]))
  }

  "Order Evaluation Steps" >> {
    "OrderEvaluateStep1" >> {
      "fail with no path" >> {
        val os = parseOrders(Seq("England: A lon - ukr"))
        val os0 = OrderEvaluateStep1.evaluate(worldMap = worldMap, os)
        os0.moves.head.mark must beSome("no convoy")
      }
      "fail with no convoy fleets" >> {
        val os = parseOrders(Seq("England: A lon - nwy"))
        val os0 = OrderEvaluateStep1.evaluate(worldMap = worldMap, os)
        os0.moves.head.mark must beSome("no convoy")
      }
      "correct convoy" >> {
        val os = parseOrders(Seq("England: A lon - nwy", "England: F nth convoys A lon - nwy"))
        val os0 = OrderEvaluateStep1.evaluate(worldMap = worldMap, os)
        os0.moves.head.mark must beNone
        os0.convoys.head.mark must beNone
      }
      "void convoy" >> {
        val os = parseOrders(Seq("England: F nth convoys A lon - nwy"))
        val os0 = OrderEvaluateStep1.evaluate(worldMap = worldMap, os)
        os0.convoys.head.mark must beSome("void")
      }

    }
  }
}

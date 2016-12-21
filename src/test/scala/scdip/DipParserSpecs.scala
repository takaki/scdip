package scdip

import org.junit.runner.RunWith
import org.specs2.matcher
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.specs2.specification.core.Fragment
import scdip.Order.MoveOrder

import scala.io.Source
import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class UnitOrderParserSpecs extends Specification with matcher.ParserMatchers {
  private val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
  override val parsers = OrderParser(variants.variant("Standard [No Units]").get)
  "UnitOrderParser" >> {
    parsers.order("England: F nth convoys A Yorkshire - yorkshire") must beASuccess
    parsers.order("England: A Yorkshire-Yorkshire") must beASuccess
    parsers.order("Germany: F london-Yorkshire") must beASuccess
    parsers.order("Germany: A Wales SUPPORTS F london-yorkshire") must beASuccess
    parsers.order("Austria: A Budapest H") must beASuccess
    parsers.order("Russia:  A Galicia-Budapest") must beASuccess
    parsers.order("Austria: F Trieste S A Budapest") must beASuccess
    parsers.order("England: F nth Convoys F london-Belgium") must beASuccess
    parsers.order("Italy: F gol-spain/sc") must beASuccess
    parsers.order("Italy: A tyr S A ven") must beASuccess
    parsers.order("England: A liverpool supports A Yorkshire-Yorkshire") must beASuccess
  }
  "check coast" >> {
    val move = parsers("Russia:  A Galicia-Budapest").right.get.asInstanceOf[MoveOrder]
    move.action.src.coast === Coast.Land
    move.action.dst.coast === Coast.Land
  }
}

@RunWith(classOf[JUnitRunner])
class DatcParserSpecs extends Specification with matcher.ParserMatchers {
  private val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
  override val parsers = DatcParser(variants.variant("Standard [No Units]").get)
  "DatcBodyParser#orders" >> {
    val txt =
      """ORDERS
        |	England: F nth convoys A Yorkshire - yorkshire
        |	England: A Yorkshire-Yorkshire
        |	England: A liverpool supports A Yorkshire-Yorkshire
        |	Germany: F london-Yorkshire
        |	Germany: A Wales SUPPORTS F london-yorkshire
        |""".stripMargin
    parsers.orders(txt) must beASuccess
  }

  "DatcBodyParser#testcase" >> {
    val txt = Source.fromInputStream(getClass.getResourceAsStream("/datc_v2.4_06.txt")).mkString
    Fragment.foreach("""CASE.*^END""".r.findAllMatchIn(txt).map(_.toString).toSeq)(t => "parse " + t.split("\n")(0) >> {
      parsers.testcase(t) must beASuccess
    })
  }

  "DatcBodyParser#datc(datc_v2.4_06.txt)" >> {
    val txt = Source.fromInputStream(getClass.getResourceAsStream("/datc_v2.4_06.txt")).mkString
    parsers.datc(txt) must beASuccess
  }

}
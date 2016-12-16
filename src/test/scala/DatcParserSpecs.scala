import org.junit.runner.RunWith
import org.specs2.matcher
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scdip.Order.MoveOrder
import scdip.{Coast, OrderParser, VariantList, WorldMap}

import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class UnitOrderParserSpecs extends Specification with matcher.ParserMatchers {
  private val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
  val parsers = OrderParser(variants.variant("Standard [No Units]").get)
  "UnitOrderParser" >> {
    parsers.order("Austria: A Budapest H") must beASuccess
    parsers.order("Russia:  A Galicia-Budapest") must beASuccess
    parsers.order("Austria: F Trieste S A Budapest") must beASuccess
    parsers.order("England: F nth Convoys F london-Belgium") must beASuccess
    parsers.order("Italy: F gol-spain/sc") must beASuccess
    parsers.order("Italy: A tyr S A ven") must beASuccess
  }
  "check coast" >> {
    val move = parsers("Russia:  A Galicia-Budapest").right.get.asInstanceOf[MoveOrder]
    move.action.src.coast === Coast.Land
    move.action.dst.coast === Coast.Land
  }
}

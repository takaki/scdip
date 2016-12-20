package scdip

import org.junit.runner.RunWith
import org.specs2.matcher
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scdip.Order.MoveOrder

import scala.io.Source
import scala.util.parsing.combinator.Parsers
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
class DatcBodyParserSpecs extends Specification with matcher.ParserMatchers {
  private val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
  override val parsers = DatcBodyParser(variants.variant("Standard [No Units]").get)
  "#orders" >> {
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

  "#testcase" >> {
    val txt =
      """CASE 6.A.5 (Move to own sector with convoy)
        |PRESTATE_SETPHASE Spring 1901, Movement
        |PRESTATE
        | England: A liverpool
        |	England: A Yorkshire
        |	England: F nth
        |	Germany: F lon
        |	Germany: A wales
        |ORDERS
        |	England: F nth convoys A Yorkshire - yorkshire
        |	England: A Yorkshire-Yorkshire
        |	England: A liverpool supports A Yorkshire-Yorkshire
        |	Germany: F london-Yorkshire
        |	Germany: A Wales SUPPORTS F london-yorkshire
        |POSTSTATE
        |	England: A liverpool
        |	England: F nth
        |	Germany: F Yorkshire
        |	Germany: A wales
        |POSTSTATE_DISLODGED
        |	England: A Yorkshire
        |END""".stripMargin
    parsers.testcase(txt) must beASuccess
    parsers.testcase(
      """CASE 6.A.10
        |PRESTATE_SETPHASE Spring 1901, Movement
        |PRESTATE
        |	Austria: A Venice
        |	Italy: F Rome
        |	Italy: A Apulia
        |ORDERS
        |	Austria: A Venice HOLD
        |	Italy: F Rome Supports A Apulia-Venice
        |	Italy: A Apulia-Venice
        |POSTSTATE_SAME
        |END""".stripMargin) must beASuccess
    parsers.testcase(
      """CASE 6.B.2
        |PRESTATE_SETPHASE Spring 1901, Movement
        |PRESTATE
        |	France: F gascony
        |ORDERS
        |	France: F gascony-spain
        |POSTSTATE
        |	France: F spain/nc
        |END""".stripMargin) must beASuccess
    parsers.testcase(
      """CASE 6.B.6
        |PRESTATE_SETPHASE Spring 1901, Movement
        |PRESTATE
        |	England: F iri
        |	England: F nao
        |	France: F spain/nc
        |	France: F mao
        |	Italy: F gol
        |ORDERS
        |	England: F iri supports F nao-mao
        |	England: F nao-mao
        |	France: F spain/nc supports F mao
        |	France: F mao hold
        |	Italy: F gol-spain/sc
        |POSTSTATE
        |	Italy: F gol
        |	France: F spain/nc
        |	England: F iri
        |	England: F mao
        |POSTSTATE_DISLODGED
        |	France: F mao
        |END""".stripMargin) must beASuccess
    parsers.testcase(
      """CASE 6.C.4
        |PRESTATE_SETPHASE Spring 1901, Movement
        |PRESTATE
        |	Austria: A tri
        |	Austria: A ser
        |	Turkey: A bul
        |	Turkey: F aeg
        |	Turkey: F ion
        |	Turkey: F adr
        |	Italy: F nap
        |ORDERS
        |	Austria: A tri-ser
        |	Austria: A ser-bul
        |	Turkey: A bul-tri
        |	Turkey: F aeg C A bul-tri
        |	Turkey: F ion C A bul-tri
        |	Turkey: F adr C A bul-tri
        |	Italy: F nap-ion
        |POSTSTATE
        |	Austria: A ser
        |	Austria: A bul
        |	Turkey: A tri
        |	Turkey: F aeg
        |	Turkey: F ion
        |	Turkey: F adr
        |	Italy: F nap
        |END""".stripMargin) must beASuccess
    parsers.testcase(
      """CASE 6.D.8
        |PRESTATE_SETPHASE Spring 1901, Movement
        |PRESTATE
        |	Austria: F ion
        |	Austria: A ser
        |	Austria: A alb
        |	Turkey: A gre
        |	Turkey: A bul
        |ORDERS
        |	Austria: F ion H
        |	Austria: A ser S A alb-gre
        |	Austria: A alb-gre
        |	Turkey: A gre-nap
        |	Turkey: A bul S A gre
        |POSTSTATE
        |	Austria: F ion
        |	Austria: A ser
        |	Austria: A gre
        |	Turkey: A bul
        |POSTSTATE_DISLODGED
        |	### Turkey: A gre		# NO RETREATS: destroyed
        |END""".stripMargin) must beASuccess
    parsers.testcase(
      """CASE 6.F.21
        |PRESTATE_SETPHASE Spring 1901, Movement
        |PRESTATE
        |	Russia: A edi
        |	Russia: F nrg
        |	Russia: A nor
        |	France: F iri
        |	France: F mao
        |	England: A lvp
        |	England: F nao
        |	England: F cly
        |ORDERS
        |	Russia: A edi S A nor-cly
        |	Russia: F nrg C A nor-cly
        |	Russia: A nor-cly
        |	France: F iri S F mao-nao
        |	France: F mao-nao
        |	England: A lvp-cly by convoy
        |	England: F nao C A lvp-cly
        |	England: F cly S F nao
        |POSTSTATE
        |	Russia: A edi
        |	Russia: F nrg
        |	Russia: A cly
        |	France: F iri
        |	France: F nao
        |	England: A lvp
        |POSTSTATE_DISLODGED
        |	# England: F nao	# DESTROYED
        |	# England: F cly	# DESTROYED
        |END""".stripMargin) must beASuccess
    parsers.testcase(
      """CASE 6.G.15. TEST CASE, BOUNCE AND DISLODGE WITH DOUBLE CONVOY
        |PRESTATE
        |	England: F nth
        |	England: A Holland
        |	England: A Yorkshire
        |	England: A London
        |	France:  F eng
        |	France:  A Belgium
        |ORDERS
        |	England: F nth Convoys A London - Belgium
        |	England: A Holland Supports A London - Belgium
        |	England: A Yorkshire - London
        |	England: A London - Belgium via Convoy
        |	France:  F eng Convoys A Belgium - London
        |	France:  A Belgium - London via Convoy
        |POSTSTATE
        |	England: F nth
        |	England: A Holland
        |	England: A Yorkshire
        |	England: A Belgium
        |	France:  F eng
        |POSTSTATE_DISLODGED
        |	France:  A Belgium
        |END""".stripMargin) must beASuccess
    parsers.testcase(
      """CASE 6.H.1
        |PRESTATE_SETPHASE Spring 1901, Retreat
        |PRESTATE
        |	Austria: A ser
        |	Italy: A ven
        |	Italy: A tri
        |	Italy: F gre
        |	Italy: F aeg
        |PRESTATE_DISLODGED
        |	Austria: F tri
        |	Turkey: F gre
        |PRESTATE_RESULTS
        |	FAILURE: Austria: F tri H
        |	SUCCESS: Austria: A ser H
        |	FAILURE: Turkey: F gre H
        |	SUCCESS: Italy: A ven S A tyr-tri
        |	SUCCESS: Italy: A tyr-tri
        |	SUCCESS: Italy: F ion-gre
        |	SUCCESS: Italy: F aeg S F ion-gre
        |ORDERS
        |	Austria: F tri-alb			# retreat
        |	Austria: A ser S F tri-alb	# this is illegal
        |	Turkey: F gre-alb			# retreat
        |POSTSTATE
        |	Austria: A ser
        |	Italy: A ven
        |	Italy: A tri
        |	Italy: F gre
        |	Italy: F aeg
        |# POSTSTATE_DISLODGED			# all dislodged units destroyed
        |END""".stripMargin) must beASuccess

  }

  "datc_v2.4_06.txt" >> {
    val txt = Source.fromInputStream(getClass.getResourceAsStream("/datc_v2.4_06.txt")).mkString
    parsers.datc(txt) must beASuccess
  }

}
package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.specs2.specification.core.Fragments
import scdip.PhaseType.Movement

import scala.io.Source
import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class DatcSpecs extends Specification {

  //  "run real.txt" >> {
  //    val txt = Source.fromInputStream(getClass.getResourceAsStream("/real.txt")).mkString
  //    val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
  //    val variant = variants.variant("Standard [No Units]").get
  //    val parsers = DatcParser(variant)
  //    val datcs = parsers.parse(txt).right.get
  //    Fragments.foreach(datcs.zipWithIndex) { case (d, i) =>
  //      s"$i ${d.title}" >> {
  //        Fragments.foreach(d.runTest)(t => t._1 >> {
  //          t._2.apply()
  //        })
  //      }
  //    }
  //  }
  "run datc_v2.4_06.txt" >> {
    val txt = Source.fromInputStream(getClass.getResourceAsStream("/datc_v2.4_06.txt")).mkString
    val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
    val variant = variants.variant("Standard [No Units]").get
    val parsers = DatcParser(variant)

    val datcs = parsers.parse(txt).right.get.filterNot(p => Set(
      "6.B.10" // TODO: wrong coast
      , "6.B.11" // TODO: wrong coast
      , "6.B.14" // TODO: adjustment
      , "6.F.17 (Pandin's extended paradox)" // TODO: wrong test case (forgetted dislodged)
      , "6.F.18" // TODO: paradox, difficult
      , "6.F.23" // TODO: paradox, difficult
      , "6.F.24" // TODO: paradox, difficult
      , "6.G.7" // TODO: neighbourconvoy, need special path check
    ).contains(p.title))
    //    Fragments.foreach(datcs.slice(0,3000))(d => d.title >> {
    val sep = 123
    // 75
    val end = 123
    "2nd" >> {
      Fragments.foreach(datcs.slice(sep, end))(d => d.title >> {
        Fragments.foreach(d.runTest)(t => t._1 >> {
          t._2.apply()
        })
      })
    }
    "1st" >> {
      Fragments.foreach(datcs.zipWithIndex.slice(0, sep)) { case (d, i) =>
        s"$i ${d.title}" >> {
          Fragments.foreach(d.runTest)(t => t._1 >> {
            t._2.apply()
          })
        }
      }
    }

  }

}

case class Datc(variant: Variant,
                title: String,
                phase: Phase,
                supplyCenterOwner: Map[Province, Power],
                preState: Seq[UnitState],
                preStateDislodged: Seq[UnitState],
                preStateResult: Seq[OrderResult],
                orders: Seq[Order],
                postState: Seq[UnitState],
                dislodged: Seq[UnitState]) extends Specification {

  def runTest = {
    if (phase.phaseType == Movement && supplyCenterOwner.isEmpty && preStateDislodged.isEmpty && preStateResult.isEmpty) {
      val iniState = variant.movementState
      val testState = iniState.copy(turn = phase.turn, unitLocation = preState.foldLeft(iniState.unitLocation)((ul, us) => ul.updated(us)))
      val retreatState = testState.next(orders).asInstanceOf[RetreatState] // TODO: FIXME
      Seq(("POSTSTATE", () => retreatState.unitLocation.unitStats.sortBy(_.location.toString) === postState.sortBy(_.location.toString)),
        ("POSTSTATE_DISLODGED", () => retreatState.dislodgeUnits.toSet === dislodged.toSet))
    } else {
      Seq(("NOT IMPLEMENTED", () => 1 === 2))
    }
  }

}


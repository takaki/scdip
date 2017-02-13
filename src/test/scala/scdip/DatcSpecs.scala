package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.specs2.specification.core.Fragments
import scdip.PhaseType.{Movement, Retreat}

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
      // ignore Civil Disorder
      "6.J.5",
      "6.J.6",
      "6.J.7",
      "6.J.8",
      "6.J.9.part1",
      "6.J.9.part2",
      "6.J.10",
      "6.J.11"
    ).contains(p.title))
    val st = 0
    val sep = st + 0
    val end = st + 200
    "2nd" >> {
      Fragments.foreach(datcs.slice(sep, end).zipWithIndex) { case (d, i) =>
        s"${i + sep} ${d.title}" >> {
          Fragments.foreach(d.runTest)(t => t._1 >> {
            t._2.apply()
          })
        }
      }
    }
    "1st" >> {
      Fragments.foreach(datcs.zipWithIndex.slice(st, sep)) { case (d, i) =>
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
                preState: Seq[UnitPosition],
                preStateDislodged: Seq[UnitPosition],
                preStateResult: Seq[OrderResult],
                orders: Seq[Order],
                postState: Seq[UnitPosition],
                dislodged: Seq[UnitPosition]) extends Specification {

  def runTest = {
    if (phase.phaseType == Movement && supplyCenterOwner.isEmpty && preStateDislodged.isEmpty && preStateResult.isEmpty) {
      val iniState = variant.movementState
      val movementState = iniState.copy(turn = phase.turn, unitLocation = preState.foldLeft(iniState.unitLocation)((ul, us) => ul.updated(us)))
      val retreatState = movementState.next(orders)
      Seq(("POSTSTATE", () => retreatState.unitLocation.units.sortBy(_.location.toString) === postState.sortBy(_.location.toString)),
        ("POSTSTATE_DISLODGED", () => retreatState.dislodgeUnits.toSet === dislodged.toSet))
    } else {
      if (phase.phaseType == Retreat) {
        val iniState = variant.movementState
        val movementOrders = preStateResult.map(_.order)
        val movementState = iniState.copy(turn = phase.turn, unitLocation = movementOrders.foldLeft(iniState.unitLocation)((ul, mo) => ul.updated(mo.unitPosition)))
        val retreatState = movementState.next(preStateResult.map(_.order))
        val nextState = retreatState.next(orders)
        Seq(("POSTSTATE", () => nextState.unitLocation.units.sortBy(_.toString) === postState.sortBy(_.toString)))
      } else {
        val iniState = variant.movementState
        val adjustmentState = AdjustmentState(iniState.worldInfo,
          phase.turn,
          preState.foldLeft(iniState.unitLocation)((ul, up) => ul.updated(up)),
          supplyCenterOwner.foldLeft(iniState.supplyCenterInfo.copy(owner = Map.empty)) { case (sc, (pr, pw)) => sc.updated(pr, pw) })
        val movementState = adjustmentState.next(orders)
        Seq(("POSTSTATE", () => movementState.unitLocation.units.sortBy(_.toString) === postState.sortBy(_.toString)))
      }
    }
  }

}


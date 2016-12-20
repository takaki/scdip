package scdip

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.specs2.specification.core.Fragment

@RunWith(classOf[JUnitRunner])
class DatcSpecs extends Specification {
  "paramtest" >> {
    Fragment.foreach(Seq("1", "2", "3"))(e => s"param test $e" >> {
      e.toInt must be_>=(1)
    })
  }
}

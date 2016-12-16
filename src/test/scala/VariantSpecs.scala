import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scdip.VariantList

import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class VariantSpecs extends Specification {
  "Variant" >> {
    val variants = VariantList(XML.load(getClass.getResourceAsStream("/variants.xml")))
    "Standard" >> {
      val standard = variants.variants.head
      standard.name === "Standard"
      standard.powers.length === 7
      standard.victoryCondition.gameLength === 35
      variants.variants.length === 12
    }
    "Standard [No Units]" >> {
      val variant = variants.variant("Standard [No Units]").get
      variant.name === "Standard [No Units]"
      variant.victoryCondition.gameLength === 35
      val eng = variant.power("England").get
      eng.name === "England"
    }
  }

}

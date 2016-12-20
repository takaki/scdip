package scdip

case class Datc(title: String,
                phase: Phase,
                supplyCenterOwner: Map[Province, Power],
                preState: Seq[UnitState],
                preStateDislodged: Seq[UnitState],
                preStateResult: Seq[OrderResult],
                orders: Seq[Order],
                postState: Seq[UnitState],
                dislodged: Seq[UnitState]) {


}

case class OrderResult(flag: Boolean, order: Order)

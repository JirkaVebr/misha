package com.mishaLang.spec.units

object Time {

	sealed trait Time extends UnitOfMeasure
	case object Second extends Time { override val symbol = "s" }
	case object MiliSecond extends Time { override val symbol = "ms" }

}

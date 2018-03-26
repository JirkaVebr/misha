package com.mishaLang.spec.units

object Time {

	sealed abstract class Time extends AtomicUnit
	case object Second extends Time { override val symbol = "s" }
	case object MiliSecond extends Time { override val symbol = "ms" }

	lazy final val TimeUnits: Map[String, Time] = Map(
		MiliSecond.symbol -> MiliSecond,
		Second.symbol -> Second
	)

}

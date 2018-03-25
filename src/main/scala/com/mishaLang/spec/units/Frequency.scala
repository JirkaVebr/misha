package com.mishaLang.spec.units

object Frequency {

	sealed trait Frequency extends UnitOfMeasure

	case object Hertz extends Frequency { override val symbol = "Hz" }
	case object KiloHertz extends Frequency { override val symbol = "kHz" }

}

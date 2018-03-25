package com.mishaLang.spec.units

object Frequency {

	sealed trait Frequency extends AtomicUnit

	case object Hertz extends Frequency { override val symbol = "Hz" }
	case object KiloHertz extends Frequency { override val symbol = "kHz" }


	lazy val frequencyUnits: Map[String, Frequency] = Map(
		Hertz.symbol -> Hertz,
		KiloHertz.symbol -> KiloHertz
	)

}
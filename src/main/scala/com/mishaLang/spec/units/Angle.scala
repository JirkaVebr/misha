package com.mishaLang.spec.units

object Angle {

	sealed trait Angle extends AtomicUnit

	case object Degree extends Angle { override val symbol = "deg" }
	case object Gradian extends Angle { override val symbol = "grad" }
	case object Radian extends Angle { override val symbol = "rad" }
	case object Turn extends Angle { override val symbol = "turn" }


	lazy final val AngleUnits: Map[String, Angle] = Map(
		Degree.symbol -> Degree,
		Gradian.symbol -> Gradian,
		Radian.symbol -> Radian,
		Turn.symbol -> Turn
	)

}

package com.mishaLang.spec.units

object Angle {

	sealed trait Angle extends UnitOfMeasure

	case object Degree extends Angle { override val symbol = "deg" }
	case object Gradian extends Angle { override val symbol = "grad" }
	case object Radian extends Angle { override val symbol = "rad" }
	case object Turn extends Angle { override val symbol = "turn" }

}

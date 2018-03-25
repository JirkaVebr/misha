package com.mishaLang.spec.units

object Flex {

	sealed trait Flex extends AtomicUnit

	case object Fraction extends Flex { override val symbol = "fr" }


	lazy final val FlexUnits: Map[String, Flex] = Map(
		Fraction.symbol -> Fraction
	)

}

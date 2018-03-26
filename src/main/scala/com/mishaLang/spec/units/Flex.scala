package com.mishaLang.spec.units

object Flex {

	sealed abstract class Flex extends AtomicUnit

	case object Fraction extends Flex { override val symbol = "fr" }


	lazy final val FlexUnits: Map[String, Flex] = Map(
		Fraction.symbol -> Fraction
	)

}

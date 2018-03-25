package com.mishaLang.spec.units

object Flex {

	sealed trait Flex extends UnitOfMeasure

	case object Fraction extends Flex { override val symbol = "fr" }

}

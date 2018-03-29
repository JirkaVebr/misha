package com.mishaLang.ast

import com.mishaLang.spec.units.AtomicUnit


object NumberUnit {


	sealed trait SimpleUnit

	case class Atomic(unit: AtomicUnit) extends SimpleUnit
	case object Percentage extends SimpleUnit

	/**
		* @example A unit of frequency could be Second with power equal to -1
		*/
	type SubUnits = Map[SimpleUnit, Int]

}

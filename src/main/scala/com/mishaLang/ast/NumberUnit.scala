package com.mishaLang.ast

import com.mishaLang.spec.units.AtomicUnit


object NumberUnit {
	sealed trait UnitOfMeasure

	sealed trait SimpleUnit extends UnitOfMeasure

	case class Atomic(unit: AtomicUnit) extends SimpleUnit
	case object Percentage extends SimpleUnit

	type SubUnits = Map[SimpleUnit, Int]

	/**
		* @example A unit of frequency could be Second with power equal to -1
		*/
	case class RaisedUnit(subUnits: SubUnits) extends UnitOfMeasure

}

package com.mishaLang.ast

import com.mishaLang.spec.units.AtomicUnit


object NumberUnit {
	sealed trait UnitOfMeasure

	sealed trait SimpleUnit extends UnitOfMeasure
	sealed trait ComplexUnit extends UnitOfMeasure


	case class Atomic(unit: AtomicUnit) extends SimpleUnit
	case object Percentage extends SimpleUnit


	/**
		* @example A unit of frequency could be Second with power equal to -1
		*/
	case class RaisedUnit(baseUnit: SimpleUnit, power: Int) extends ComplexUnit
	case class RaisedUnitProduct(subUnits: Set[RaisedUnit]) extends ComplexUnit

}

package com.mishaLang.ast


object NumberUnit {
	sealed abstract class Unit

	/**
		*
		* @param subUnitProduct A map from sub-unit names to powers. For instance, the SI unit of
		*                       acceleration would be <code>Map("m" -> 1, "s" -> -2)</code>.
		*/
	case class UnitOfMeasure(subUnitProduct: Map[String, Int] = Map.empty) extends Unit

	case object Percentage extends Unit
}

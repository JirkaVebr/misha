package com.preprocessor.ast


object UnitOfMeasure {
	sealed abstract class UnitOfMeasure

	/**
		*
		* @param subUnitProduct A map from sub-unit names to powers. For instance, the SI unit of
		*                       acceleration would be <code>Map("m" -> 1, "s" -> -2)</code>.
		*/
	final case class GenericUnit(subUnitProduct: Map[String, Int] = Map.empty) extends UnitOfMeasure

	final case object Percentage extends UnitOfMeasure

	final case object Scalar extends UnitOfMeasure
}

package com.mishaLang.interpreter.validators

import com.mishaLang.ast.Language.Value
import com.mishaLang.interpreter.ops.UnitOps

object NumberValidator {
	object Integer {
		def unapply(number: Double): Boolean = isInteger(number)
	}

	def isInteger(number: Double): Boolean =
		number.abs <= Double.MaxValue && number.floor == number

	def isInteger(number: Value.Number): Boolean = isInteger(number.value)

	def isScalar(number: Value.Number): Boolean =
		number.unit.isEmpty

	def isPercentage(number: Value.Number): Boolean =
		UnitOps.isPercentage(number.unit)

}

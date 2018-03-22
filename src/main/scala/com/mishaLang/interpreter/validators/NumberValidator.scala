package com.mishaLang.interpreter.validators

import com.mishaLang.ast.Language.Value.{Percentage, Scalar}

object NumberValidator {

	def isInteger(number: Double): Boolean =
		number.abs <= Double.MaxValue && number.floor == number

	def isInteger(number: Scalar): Boolean = isInteger(number.value)
	def isInteger(number: Percentage): Boolean = isInteger(number.value)

}

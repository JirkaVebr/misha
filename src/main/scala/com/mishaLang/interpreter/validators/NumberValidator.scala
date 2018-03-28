package com.mishaLang.interpreter.validators

import com.mishaLang.ast.Language.Value

object NumberValidator {

	def isInteger(number: Double): Boolean =
		number.abs <= Double.MaxValue && number.floor == number

	def isInteger(number: Value.Number): Boolean = isInteger(number.value)

}

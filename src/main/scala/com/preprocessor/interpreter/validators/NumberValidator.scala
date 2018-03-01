package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Ast.Value.{Percentage, Scalar}

object NumberValidator {

	def isInteger(number: Double): Boolean =
		number.abs <= Double.MaxValue && number.floor == number

	def isInteger(number: Scalar): Boolean = isInteger(number.value)
	def isInteger(number: Percentage): Boolean = isInteger(number.value)

}

package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Ast.Value.Scalar

object NumberValidator {

	def isInteger(number: Scalar): Boolean =
		number.value.abs <= Double.MaxValue && number.value.floor == number.value

}

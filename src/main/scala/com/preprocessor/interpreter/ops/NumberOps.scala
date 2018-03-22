package com.preprocessor.interpreter.ops

import com.preprocessor.ast.Language.Value
import com.preprocessor.interpreter.validators.NumberValidator

object NumberOps {



	// Properties

	def isEven(number: Value.Number): Value.Boolean =
		Value.Boolean(number.value % 2 == 0)

	def isNegative(number: Value.Number): Value.Boolean =
		Value.Boolean(number.value < 0)

	def isOdd(number: Value.Number): Value.Boolean =
		Value.Boolean(number.value % 2 != 0)

	def isPositive(number: Value.Number): Value.Boolean =
		Value.Boolean(number.value > 0)

	def isWhole(number: Value.Number): Value.Boolean =
		Value.Boolean(NumberValidator.isInteger(number.value))

	def toPercentage(number: Value.Number): Value.Percentage =
		Value.Percentage(number.value)

	def toScalar(number: Value.Number): Value.Scalar =
		Value.Scalar(number.value)

}

package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.Scalar
import com.mishaLang.interpreter.validators.NumberValidator

object NumberOps {


	def performNumericOperator(operator: NumericOperator, left: Scalar, right: Scalar): Scalar = {
		val performOperation: (Double, Double) => Double = operator match {
			case Addition => _ + _
			case Subtraction => _ - _
			case Multiplication => _ * _
			case Division => _ / _
			case Exponentiation => Math.pow(_, _)
			case Remainder => _ % _
		}
		Scalar(performOperation(left.value, right.value))
	}



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

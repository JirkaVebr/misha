package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Percentage, Scalar}
import com.mishaLang.interpreter.validators.NumberValidator

object NumberOps {


	private def performOperation(operator: NumericOperator, left: Double, right: Double): Double = operator match {
		case Addition => left + right
		case Subtraction => left - right
		case Multiplication => left * right
		case Division => left / right
		case Exponentiation => Math.pow(left, right)
		case Remainder => left % right
	}

	def performNumericOperator(operator: NumericOperator, left: Scalar, right: Scalar): Scalar = {
		Scalar(performOperation(operator, left.value, right.value))
	}
	def performNumericOperator(operator: NumericOperator, left: Percentage, right: Percentage): Percentage = {
		Percentage(performOperation(operator, left.value, right.value))
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

package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.Scalar
import com.mishaLang.ast.NumberUnit.Percentage
import com.mishaLang.interpreter.validators.NumberValidator
import com.mishaLang.utils.MathUtils

object NumberOps {

	final val NumericPrecision = 6


	def normalizeDouble(double: Double, precision: Int = NumericPrecision): Double =
		if (precision == 0) double.round
		else MathUtils.round(double, precision)


	def formatDouble(double: Double, precision: Int = NumericPrecision): String =
		if (NumberValidator.isInteger(double)) double.toInt.toString
		else normalizeDouble(double, precision).toString


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
	def performNumericOperator(operator: NumericOperator, left: Value.Dimensioned, right: Value.Dimensioned): Value.Number = {
		Value.Dimensioned(performOperation(operator, left.value, right.value), ???) // TODO
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

	def toPercentage(number: Value.Number): Value.Dimensioned =
		Value.Dimensioned(number.value, Percentage)

	def toScalar(number: Value.Number): Value.Scalar =
		Value.Scalar(number.value)

}

package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Dimensioned, Formula, Scalar}
import com.mishaLang.ast.NumberUnit._
import com.mishaLang.ast.SimpleExpression
import com.mishaLang.ast.SimpleExpression.{SimpleExpression, Term}
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

	def performNumericOperator(operator: NumericOperator, left: Dimensioned, right: Dimensioned): Option[Value.Value] = {
		val leftUnit: RaisedUnit = UnitOps.raiseUnit(left.unit)
		val rightUnit: RaisedUnit = UnitOps.raiseUnit(right.unit)

		operator match {
			case simple: SimpleNumericOperator => simple match {
				case Addition | Subtraction =>
					if (leftUnit == rightUnit)
						Some(UnitOps.normalizeDimensioned(
							performOperation(operator, left.value, right.value), leftUnit
						))
					else
						Some(Formula(SimpleExpression.BinaryOperation(simple, Term(left), Term(right))))

				case Multiplication =>
					Some(UnitOps.normalizeDimensioned(
						left.value * right.value, UnitOps.addUnits(leftUnit, rightUnit)
					))
				case Division =>
					Some(UnitOps.normalizeDimensioned(
						left.value / right.value,
						UnitOps.addUnits(leftUnit, UnitOps.multiplyUnit(rightUnit, -1))
					))
			}
			case _: ComplexNumericOperator => None
		}
	}

	def performNumericOperator(operator: NumericOperator, left: Dimensioned, right: Scalar): Option[Value.Value] =
		operator match {
			case simple: SimpleNumericOperator => simple match {
				case Addition | Subtraction =>
					Some(Formula(SimpleExpression.BinaryOperation(simple, Term(left), Term(right))))
				case Multiplication | Division =>
					Some(Dimensioned(performOperation(simple, left.value, right.value), left.unit))
			}
			case complex: ComplexNumericOperator =>
				if (NumberValidator.isInteger(right)) {
					val rightInteger = right.value.toInt

					complex match {
						case Exponentiation =>
							Some(UnitOps.normalizeDimensioned(
								Math.pow(left.value, rightInteger),
								UnitOps.multiplyUnit(UnitOps.raiseUnit(left.unit), rightInteger)
							))
						case Remainder =>
							Some(Dimensioned(left.value % rightInteger, left.unit))
					}
				} else
					None
		}


	def simplifyFormula(formula: Formula): Value.Value = {
		def simplify(expression: SimpleExpression): SimpleExpression =
			expression // TODO actually simplify...
		simplify(formula.formula) match {
			case operation: SimpleExpression.BinaryOperation => Value.Formula(operation)
			case Term(value) => value
		}
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

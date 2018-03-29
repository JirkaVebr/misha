package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Formula, Number}
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


	private def performOperation(operator: SimpleNumericOperator, left: Double, right: Double): Double = operator match {
		case Addition => left + right
		case Subtraction => left - right
		case Multiplication => left * right
		case Division => left / right
	}

	def performNumericOperator(operator: NumericOperator, left: Number, right: Number): Option[Value.Value] = {
		val leftUnit: SubUnits = left.unit
		val rightUnit: SubUnits = right.unit

		operator match {
			case simple: SimpleNumericOperator => simple match {
				case Addition | Subtraction =>
					if (leftUnit == rightUnit)
						Some(UnitOps.normalizeUnit(
							performOperation(simple, left.value, right.value), leftUnit
						))
					else
						Some(simplifyFormula(Formula(SimpleExpression.BinaryOperation(simple, Term(left), Term(right)))))

				case Multiplication =>
					Some(UnitOps.normalizeUnit(
						left.value * right.value, UnitOps.addUnits(leftUnit, rightUnit)
					))
				case Division =>
					Some(UnitOps.normalizeUnit(
						left.value / right.value,
						UnitOps.addUnits(leftUnit, UnitOps.multiplyUnit(rightUnit, -1))
					))
			}
			case complex: ComplexNumericOperator =>
				if (NumberValidator.isScalar(right)) {
					complex match {
						case Exponentiation =>
							pow(left, right)
						case Remainder =>
							if (NumberValidator.isInteger(right))
								Some(Number(left.value % right.value.toInt, left.unit))
							else None
					}
				} else
					None
		}
	}


	def simplifyFormula(formula: Formula): Value.Value = {
		def simplify(expression: SimpleExpression): SimpleExpression =
			expression // TODO actually simplify...
		simplify(formula.formula) match {
			case operation: SimpleExpression.BinaryOperation => Value.Formula(operation)
			case Term(value) => value
		}
	}


	def pow(base: Value.Number, exponent: Value.Number): Option[Value.Number] =
		if (NumberValidator.isScalar(exponent)) {
			val legalExponent = base.unit.forall {
				case (_, unitExponent) => NumberValidator.isInteger(exponent.value * unitExponent)
			}
			if (legalExponent)
				Some(UnitOps.normalizeUnit(
					Math.pow(base.value, exponent.value),
					UnitOps.multiplyUnit(base.unit, exponent.value)
				))
			else
				None
		} else
			None



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

	def sqrt(number: Value.Number): Option[Value.Number] =
		pow(number, Value.Number(.5))

	def toPercentage(number: Number): Number =
		Number(number.value, Percentage)

	def toScalar(number: Value.Number): Number =
		Number(number.value)

}

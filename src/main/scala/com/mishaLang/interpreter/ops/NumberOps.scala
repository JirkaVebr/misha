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


	private def performOperation(operator: NumericOperator, left: Double, right: Double): Double = operator match {
		case Addition => left + right
		case Subtraction => left - right
		case Multiplication => left * right
		case Division => left / right
		case Exponentiation => Math.pow(left, right)
		case Remainder => left % right
	}

	def performNumericOperator(operator: NumericOperator, left: Number, right: Number): Option[Value.Value] = {
		val leftUnit: SubUnits = left.unit
		val rightUnit: SubUnits = right.unit

		operator match {
			case simple: SimpleNumericOperator => simple match {
				case Addition | Subtraction =>
					if (leftUnit == rightUnit)
						Some(UnitOps.normalizeUnit(
							performOperation(operator, left.value, right.value), leftUnit
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
				if (NumberValidator.isInteger(right) && NumberValidator.isScalar(right)) {
					val rightInteger = right.value.toInt

					complex match {
						case Exponentiation =>
							Some(UnitOps.normalizeUnit(
								Math.pow(left.value, rightInteger),
								UnitOps.multiplyUnit(left.unit, rightInteger)
							))
						case Remainder =>
							Some(Number(left.value % rightInteger, left.unit))
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
		???
		/*number match {
			case Dimensioned(value, unit) =>
				val raisedUnit = UnitOps.raiseUnit(unit)
				val isSquare = raisedUnit.subUnits.forall {
					case (_, exponent) => exponent % 2 == 0
				}
				if (isSquare)
					Some(UnitOps.normalizeDimensioned(
						Math.sqrt(value), UnitOps.multiplyUnit(raisedUnit, 0.5)
					))
				else
					None
			case Scalar(value) =>
				Some(Scalar(Math.sqrt(value)))
		}*/

	def toPercentage(number: Number): Number =
		Number(number.value, Percentage)

	def toScalar(number: Value.Number): Number =
		Number(number.value)

}

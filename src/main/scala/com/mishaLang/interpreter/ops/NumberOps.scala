package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Dimensioned, Formula, Scalar}
import com.mishaLang.ast.NumberUnit._
import com.mishaLang.ast.SimpleExpression
import com.mishaLang.ast.SimpleExpression.{SimpleExpression, Term}
import com.mishaLang.interpreter.validators.NumberValidator
import com.mishaLang.utils.MathUtils

import scala.annotation.tailrec

object NumberOps {

	final val NumericPrecision = 6


	def normalizeDouble(double: Double, precision: Int = NumericPrecision): Double =
		if (precision == 0) double.round
		else MathUtils.round(double, precision)


	def formatDouble(double: Double, precision: Int = NumericPrecision): String =
		if (NumberValidator.isInteger(double)) double.toInt.toString
		else normalizeDouble(double, precision).toString


	def normalizeDimensioned(value: Double, unit: UnitOfMeasure): Value.Number =
		unit match {
			case _: SimpleUnit => Dimensioned(value, unit)
			case raised: RaisedUnit =>
				if (raised.subUnits.isEmpty)
					Scalar(value)
				else {
					val conclusive: Option[Value.Number] =
						if (raised.subUnits.size == 1)
							raised.subUnits.head match {
								case (Percentage, 1) => Some(Dimensioned(value, Percentage))
								case (atomic: Atomic, 1) => Some(Dimensioned(value, atomic))
								case _ => None
							}
						else None
					conclusive match {
						case Some(number) => number
						case None =>
							val newSubUnits = raised.subUnits.filterNot {
								case (_, exponent) => exponent == 0
							}
							if (newSubUnits.isEmpty) Scalar(value)
							else Dimensioned(value, unit)
					}
				}
		}

	def raiseUnit(unit: UnitOfMeasure): RaisedUnit =
		unit match {
			case simple: SimpleUnit => RaisedUnit(Map(simple -> 1))
			case raised: RaisedUnit => raised
		}


	def addUnits(left: RaisedUnit, right: RaisedUnit): RaisedUnit = {
		@tailrec def add(leftUnits: SubUnits, rightUnits: SubUnits): SubUnits = {
			if (rightUnits.isEmpty) leftUnits
			else {
				val head = rightUnits.head
				val newExponent = head._2 + leftUnits.getOrElse(head._1, 0)
				val tail = rightUnits.tail

				if (newExponent == 0) add(leftUnits - head._1, tail)
				else add(leftUnits.updated(head._1, newExponent), tail)
			}
		}
		val added = add(left.subUnits, right.subUnits)
		RaisedUnit(added)
	}

	def multiplyUnit(unit: RaisedUnit, factor: Int): RaisedUnit =
		RaisedUnit(unit.subUnits.map {
			case (subUnit, exponent) => (subUnit, factor * exponent)
		})


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
		val leftUnit: RaisedUnit = raiseUnit(left.unit)
		val rightUnit: RaisedUnit = raiseUnit(right.unit)

		operator match {
			case simple: SimpleNumericOperator => simple match {
				case Addition | Subtraction =>
					if (leftUnit == rightUnit)
						Some(normalizeDimensioned(
							performOperation(operator, left.value, right.value), leftUnit
						))
					else
						Some(Formula(SimpleExpression.BinaryOperation(simple, Term(left), Term(right))))

				case Multiplication =>
					Some(normalizeDimensioned(
						left.value * right.value, addUnits(leftUnit, rightUnit)
					))
				case Division =>
					Some(normalizeDimensioned(
						left.value / right.value,
						addUnits(leftUnit, multiplyUnit(rightUnit, -1))
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
							Some(normalizeDimensioned(
								Math.pow(left.value, rightInteger),
								multiplyUnit(raiseUnit(left.unit), rightInteger)
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

package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value.{Dimensioned, Native, Number, Primitive, Rgba, Scalar, Value}
import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.NumberUnit
import com.mishaLang.ast.NumberUnit.{Atomic, ComplexUnit, SimpleUnit}
import com.mishaLang.error.NativeError
import com.mishaLang.error.NativeError.StringIndexOutOfBounds
import com.mishaLang.interpreter.validators.NumberValidator
import com.mishaLang.spec.units.Length.Length

import scala.util.{Failure, Success}

object StringOps {

	private def numberToString(number: Double): String =
		if (NumberValidator.isInteger(number)) number.toInt.toString
		else number.toString


	def castToString(value: Value.Value): Option[Value.String] = value match {
		case primitive: Primitive => primitive match {
			case Value.String(string) => Some(Value.String(string))
			case number: Number => number match {
				case Scalar(magnitude) => Some(Value.String(NumberOps.formatDouble(magnitude)))
				case Dimensioned(magnitude, unit) => unit match {
					case simple: SimpleUnit => simple match {
						case Atomic(atomicUnit) =>
							atomicUnit match {
								case lengthUnit: Length =>
									if (magnitude == 0d) Some(Value.String("0"))
									else Some(Value.String(NumberOps.formatDouble(magnitude) + lengthUnit.symbol))
								case _ =>
									Some(Value.String(NumberOps.formatDouble(magnitude) + atomicUnit.symbol))
							}
						case NumberUnit.Percentage =>
							Some(Value.String(NumberOps.formatDouble(magnitude) + '%'))
					}
					case _: ComplexUnit => None
				}
			}
			case color: Rgba => Some(ColorOps.toString(color))
			case _ => None
		}
		case _ => None
	}

	def concatenate(first: Value.String, second: Value.String): Value.String =
		Value.String(first.value + second.value)

	/**
		* Be sure to check NumberValidator.isInteger(factor) beforehand.
		*/
	def multiply(string: Value.String, factor: Scalar): Value.String =
		Value.String(string.value * factor.value.toInt)



	// Properties

	def length(string: Value.String): Value.Scalar =
		Value.Scalar(string.value.length)

	def toLowerCase(string: Value.String): Value.String =
		Value.String(string.value.toLowerCase)

	def toUpperCase(string: Value.String): Value.String =
		Value.String(string.value.toUpperCase)

	def trim(string: Value.String): Value.String =
		Value.String(string.value.trim)


	// Method generators

	def getCharAt(string: Value.String): Native =
		Native(Vector(Type.Scalar), (arguments: Vector[Value]) => {
			val position = arguments(0).asInstanceOf[Scalar].value

			if (position >= string.value.length || position < 0)
				Failure(NativeError(StringIndexOutOfBounds))
			else
				Success(Value.String(string.value.charAt(position.toInt).toString))
		})

	def getConcat(string: Value.String): Native =
		Native(Vector(Type.String), (arguments: Vector[Value]) => {
			val otherString = arguments(0).asInstanceOf[Value.String].value

			Success(concatenate(string, otherString))
		})

	def getEndsWith(string: Value.String): Native =
		Native(Vector(Type.String), (arguments: Vector[Value]) => {
			val otherString = arguments(0).asInstanceOf[Value.String].value

			Success(Value.Boolean(string.value.endsWith(otherString)))
		})

}

package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value.{Native, Number, Primitive, Rgba, Value}
import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.NumberUnit.{Atomic, Percentage}
import com.mishaLang.error.NativeError
import com.mishaLang.error.NativeError.StringIndexOutOfBounds
import com.mishaLang.spec.units.Length.Length

import scala.util.{Failure, Success}

object StringOps {


	def castToString(value: Value.Value): Option[Value.String] = value match {
		case primitive: Primitive => primitive match {
			case Value.String(string) => Some(Value.String(string))
			case number: Number =>
				val baseOutput =  NumberOps.formatDouble(number.value)

				if (number.unit.isEmpty)
					Some(baseOutput)
				else if (UnitOps.isAtomicUnit(number.unit)) {
					val atomic = number.unit.head._1

					atomic match {
						case Atomic(unit) => unit match {
							case _: Length if number.value == 0d =>
								Some(baseOutput)
							case _ =>
								Some(baseOutput + unit.symbol)
						}
						case Percentage =>
							Some(baseOutput + '%')
					}
				} else None
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
	def multiply(string: Value.String, factor: Number): Value.String =
		Value.String(string.value * factor.value.toInt)



	// Properties

	def length(string: Value.String): Value.Number =
		Value.Number(string.value.length)

	def toLowerCase(string: Value.String): Value.String =
		Value.String(string.value.toLowerCase)

	def toUpperCase(string: Value.String): Value.String =
		Value.String(string.value.toUpperCase)

	def trim(string: Value.String): Value.String =
		Value.String(string.value.trim)


	// Method generators

	def getCharAt(string: Value.String): Native =
		Native(Vector(Type.Scalar), (arguments: Vector[Value]) => {
			val position = arguments(0).asInstanceOf[Number].value

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

	def getStartsWith(string: Value.String): Native =
		Native(Vector(Type.String), (arguments: Vector[Value]) => {
			val otherString = arguments(0).asInstanceOf[Value.String].value

			Success(Value.Boolean(string.value.startsWith(otherString)))
		})

}

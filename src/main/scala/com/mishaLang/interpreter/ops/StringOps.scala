package com.mishaLang.interpreter.ops

import java.util.regex.Pattern

import com.mishaLang.ast.Language.Value.{Native, Number, PolymorphicGroup, Primitive, Rgba}
import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.NumberUnit.{Atomic, Percentage}
import com.mishaLang.error.NativeError
import com.mishaLang.error.NativeError.StringIndexOutOfBounds
import com.mishaLang.interpreter.validators.NumberValidator
import com.mishaLang.spec.units.Length.Length

import scala.util.{Failure, Success, Try}

object StringOps {


	def castToString(value: Value.Value): Option[Value.String] = value match {
		case primitive: Primitive => primitive match {
			case Value.String(string) => Some(Value.String(string))
			case number: Number =>
				val baseOutput = NumberOps.formatDouble(number.value)

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
		Native(Vector(Type.Scalar), {
			case Vector(Value.Number(position, _)) =>
				if (position >= string.value.length || position < 0)
					Failure(NativeError(StringIndexOutOfBounds))
				else
					Success(Value.String(string.value.charAt(position.toInt).toString))
		})

	def getConcat(string: Value.String): Native =
		Native(Vector(Type.String), {
			case Vector(Value.String(otherString)) => Success(concatenate(string, otherString))
		})

	def getEndsWith(string: Value.String): Native =
		Native(Vector(Type.String), {
			case Vector(Value.String(otherString)) => Success(Value.Boolean(string.value.endsWith(otherString)))
		})

	def getStartsWith(string: Value.String): Native =
		Native(Vector(Type.String), {
			case Vector(Value.String(otherString)) => Success(Value.Boolean(string.value.startsWith(otherString)))
		})

	def getSplit(string: Value.String): Native =
		Native(Vector(Type.String), {
			case Vector(Value.String(delimiter)) =>
				Success(Value.List(string.value.split(delimiter, -1).map(Value.String).toVector))
		})

	def getReplace(string: Value.String): Native =
		Native(Vector(Type.String, Type.String), {
			case Vector(Value.String(oldChar), Value.String(newChar)) =>
				Success(Value.String(oldChar.r.replaceAllIn(string.value, newChar)))
		})

	def getIndexOf(haystack: Value.String): Native =
		Native(Vector(Type.String), {
			case Vector(Value.String(needle)) => Success(Value.Number(haystack.value.indexOf(needle)))
		})

	def getRepeat(string: Value.String): Native =
		Native(Vector(Type.Scalar), {
			case Vector(num: Value.Number) => num.value match {
				case NumberValidator.Integer() => Success(multiply(string, num))
				case _ => Failure(NativeError(NativeError.ArgumentIsNotInteger("multiplier")))
			}
		})

	def getSubstring(string: Value.String): PolymorphicGroup = {
		def substring(start: Double, end: Double): Try[Value.String] = {
			if (!NumberValidator.isInteger(start))
				Failure(NativeError(NativeError.ArgumentIsNotInteger("start")))
			else if (!NumberValidator.isInteger(end))
				Failure(NativeError(NativeError.ArgumentIsNotInteger("end")))
			else if(start < 0 || start > string.value.length)
				Failure(NativeError(NativeError.ArgumentIsOutOfBounds("start")))
			else if (end < start)
				Failure(NativeError(NativeError.ArgumentIsOutOfBounds("end")))
			else
				Success(string.value.substring(start.toInt, end.toInt))
		}

		PolymorphicGroup(Seq(
			Native(Vector(Type.Scalar, Type.Scalar), {
				case Vector(Value.Number(start, _), Value.Number(end, _)) =>
					substring(start, end)
			}),
			Native(Vector(Type.Scalar), {
				case Vector(Value.Number(start, _)) =>
					substring(start, string.value.length)
			})
		))
	}


}

package com.preprocessor.interpreter.ops

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{Dimensioned, Number, Percentage, Primitive, Scalar}
import com.preprocessor.interpreter.validators.NumberValidator

object StringOps {

	private def numberToString(number: Double): String =
		if (NumberValidator.isInteger(number)) number.toInt.toString
		else number.toString


	def castToString(value: Value.Value): Option[Value.String] = value match {
		case primitive: Primitive => primitive match {
			case Value.String(string) => Some(Value.String(string))
			case number: Number => number match {
				case Scalar(magnitude) => Some(Value.String(numberToString(magnitude)))
				case Percentage(magnitude) => Some(Value.String(numberToString(magnitude) + '%'))
				case Dimensioned(_, _) => None // TODO?
			}
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


}

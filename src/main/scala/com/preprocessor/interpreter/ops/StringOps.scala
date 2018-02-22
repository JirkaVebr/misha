package com.preprocessor.interpreter.ops

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{Dimensioned, Number, Percentage, Primitive, Scalar, String}

object StringOps {


	def castToString(value: Value.Value): Option[Value.String] = value match {
		case primitive: Primitive => primitive match {
			case String(string) => Some(Value.String(string))
			case number: Number => number match {
				case Scalar(value) => Some(Value.String(value.toString))
				case Percentage(value) => Some(Value.String(value.toString + '%'))
				case Dimensioned(value, unit) => None
			}
			case _ => None
		}
		case _ => None
	}

	def concatenate(first: Value.String, second: Value.String): Value.String =
		Value.String(first.value + second.value)

}

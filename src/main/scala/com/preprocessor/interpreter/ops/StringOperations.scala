package com.preprocessor.interpreter.ops

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{Number, Primitive, String}
import com.preprocessor.ast.UnitOfMeasure.Scalar

object StringOperations {


	def castToString(value: Value.Value): Option[Value.String] = value match {
		case primitive: Primitive => primitive match {
			case String(string) => Some(Value.String(string))
			case Number(number, unit) => unit match {
				case Scalar() => Some(Value.String(number.toString))
				case _ => None
			}
			case _ => None
		}
		case _ => None
	}

	def concatenate(first: Value.String, second: Value.String): Value.String =
		Value.String(first.value + second.value)

}

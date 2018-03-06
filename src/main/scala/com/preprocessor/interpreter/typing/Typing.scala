package com.preprocessor.interpreter.typing

import com.preprocessor.ast.Language.Value.{Boolean, Color, Composite, Dimensioned, Flag, Function, Number, Percentage, Primitive, Scalar, String, Tuple2, Unit}
import com.preprocessor.ast.Language.{Type, Value}
import com.preprocessor.interpreter.Environment

object Typing {

	def getType(value: Value.Value)/*(implicit environment: Environment)*/: Type.Any = value match {
		case Unit => Type.Unit
		case primitive: Primitive => primitive match {
			case number: Number => number match {
				case Dimensioned(value, unit) => sys.error("todo") // TODO
				case Scalar(_) => Type.Scalar
				case Percentage(_) => Type.Percentage
			}
			case Boolean(_) => Type.Boolean
			case String(_) => Type.String
			case _: Color => Type.Color
			case _: Flag => Type.Flag
		}
		case composite: Composite => composite match {
			case Tuple2(first, second) => Type.Tuple2(getType(first), getType(second))
			case Value.List(values) =>  sys.error("todo") // TODO
			case Function(arguments, returnType, body) =>  sys.error("todo") // TODO
		}
	}
}

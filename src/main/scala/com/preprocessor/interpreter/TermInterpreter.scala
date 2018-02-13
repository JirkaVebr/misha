package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Term.{FunctionCall, MemberAccess, Term, Variable}
import com.preprocessor.ast.Ast.{Term, Type, Value}
import com.preprocessor.ast.Ast.Value._

object TermInterpreter {

	def run(term: Term)(implicit state: EvalState): EvalState = term match {
		case value: Value => value match {
			case Value.Unit => state ~> (Type.Unit, Value.Unit)
			case primitiveValue: Primitive => primitiveValue match {
				case Number(number, unit) => sys.error("todo") // TODO check dimensions
				case Boolean(_) => state ~> (Type.Boolean, primitiveValue)
				case String(string) => sys.error("todo") // TODO replace &
				case _: Color => state ~> (Type.Color, primitiveValue)
				case _: Flag => state ~> (Type.Flag, primitiveValue)
			}
			case compositeValue: Composite => compositeValue match {
				case Tuple2Value(first, second) => sys.error("todo") // TODO
				case List(values) => sys.error("todo") // TODO find common supertype
			}
		}
		case Variable(name) => state
		case FunctionCall(function, arguments) => sys.error("todo") // TODO
		case Term.List(items) => sys.error("todo") // TODO
		case MemberAccess(container, name) => sys.error("todo") // TODO
	}
}

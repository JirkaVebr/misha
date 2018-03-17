package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Term.MemberAccess
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Language.Value.{Boolean, Color, Composite, Flag, Number, Primitive, Unit}
import com.preprocessor.error.CompilerError
import com.preprocessor.error.ProgramError.NonStringMemberCastFail
import com.preprocessor.interpreter.ops.StringOps

import scala.util.{Failure, Success, Try}

object MemberAccessInterpreter {

	def run(memberAccess: MemberAccess)(implicit state: EvalState): Try[EvalState] =
		Interpreter.chainRun[Expression](List(
			memberAccess.container, memberAccess.name
		), state, ExpressionInterpreter.run(_)(_)) match {
			case Failure(exception) => Failure(exception)
			case Success((container :: name :: Nil, newState)) =>
				val memberName: Option[String] = name.value match {
					case Value.String(member) => Some(member)
					case key => StringOps.castToString(key) match {
						case Some(stringMember) => Some(stringMember.value)
						case None => None
					}
				}
				memberName match {
					case Some(string) => container.value match {
						case Unit => ???
						case primitive: Primitive => runPrimitive(primitive, string)(newState)
						case _: Composite => ???
					}
					case None => state.fail(NonStringMemberCastFail, memberAccess)
				}
			case _ => state.failFatally(CompilerError("")) // TODO
		}


	private def runPrimitive(primitive: Primitive, memberName: String)(implicit state: EvalState): Try[EvalState] =
		primitive match {
			case _: Number => ???
			case Boolean(value) => ???
			case string: Value.String => runString(string, memberName)
			case _: Color => ???
			case _: Flag => ???
		}


	private def runString(string: Value.String, memberName: String)(implicit state: EvalState): Try[EvalState] = {
		val result = memberName match {
			case "charAt" => ???
			case "concat" => ???
			case "endsWith" => ???
			case "indexOf" => ???
			case "length" => Some(StringOps.length(string))
			case "split" => ???
			case "substring" => ???
			case "toLowerCase" => Some(StringOps.toLowerCase(string))
			case "toString" => Some(string)
			case "toUpperCase" => Some(StringOps.toUpperCase(string))
			case "trim" => Some(StringOps.trim(string))
			case _ => None
		}
		result match {
			case Some(x) => state.evaluatedTo(x)
			case None => ???
		}
	}

}

package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Term.MemberAccess
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Language.Value.{Boolean, Color, Composite, CurrentColor, Dimensioned, Flag, NativeFunctionCall, Number, Percentage, Primitive, Rgba, Scalar, Transparent, Unit}
import com.preprocessor.error.CompilerError
import com.preprocessor.error.ProgramError.NonStringMemberCastFail
import com.preprocessor.interpreter.ops.StringOps
import com.preprocessor.interpreter.validators.NumberValidator

import scala.util.{Failure, Success, Try}

object MemberAccessInterpreter {

	def run(memberAccess: MemberAccess)(implicit state: EnvWithValue): Try[EnvWithValue] =
		Interpreter.chainRun[Expression](List(
			memberAccess.container, memberAccess.name
		), state, ExpressionInterpreter.run(_)(_)) match {
			case Failure(exception) => Failure(exception)
			case Success(newEnvironment) =>
				val (container :: name :: Nil) = newEnvironment.value
				val newState = EnvironmentWithValue(newEnvironment.environment)

				val memberName: Option[String] = name match {
					case Value.String(member) => Some(member)
					case key => StringOps.castToString(key) match {
						case Some(stringMember) => Some(stringMember.value)
						case None => None
					}
				}
				memberName match {
					case Some(string) => container match {
						case Unit => ???
						case primitive: Primitive => runPrimitive(primitive, string)(newState)
						case _: Composite => ???
					}
					case None => state.fail(NonStringMemberCastFail, memberAccess)
				}
			case _ => state.failFatally(CompilerError("")) // TODO
		}


	private def runPrimitive(primitive: Primitive, memberName: String)(implicit state: EnvWithValue): Try[EnvWithValue] =
		primitive match {
			case number: Number => runNumber(number, memberName)
			case Boolean(value) => ???
			case string: Value.String => runString(string, memberName)
			case color: Color => ???
			case _: Flag => ???
			case _: NativeFunctionCall => ???
		}


	private def runNumber(number: Value.Number, memberName: String)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val commonResult = memberName match {
			case "isEven" => Some(Value.Boolean(number.value % 2 == 0))
			case "isNegative" => Some(Value.Boolean(number.value < 0))
			case "isOdd" => Some(Value.Boolean(number.value % 2 != 0))
			case "isPositive" => Some(Value.Boolean(number.value > 0))
			case "isWhole" => Some(Value.Boolean(NumberValidator.isInteger(number.value)))
			case "toPercentage" => Some(Value.Percentage(number.value))
			case "toScalar" => Some(Value.Scalar(number.value))
			case "toString" => StringOps.castToString(number)
			case _ => {
				val newValue = memberName match { // TODO
					case "abs" => number.value.abs
					case "ceil" => number.value.ceil
					case "floor" => number.value.floor
					case "round" => number.value.round
				}
				None
			}
		}
		commonResult match {
			case Some(x) => state ~> x
			case None => ???
		}
	}


	private def runString(string: Value.String, memberName: String)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val result = memberName match {
			case "charAt" => Some(StringOps.getCharAt(string))
			case "concat" => Some(StringOps.getConcat(string))
			case "endsWith" => Some(StringOps.getEndsWith(string))
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
			case Some(x) => state ~> x
			case None => ???
		}
	}


	private def runColor(color: Value.Color, memberName: String)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val result = color match {
			case Rgba(r, g, b, a) => memberName match {
				case "adjustHue" => ???
				case "alpha" => Some(Value.Scalar(a))
				case "blue" => Some(Value.Scalar(b))
				case "complement" => ???
				case "darken" => ???
				case "desaturate" => ???
				case "green" => Some(Value.Scalar(g))
				case "hue" => ???
				case "inverted" => ???
				case "isDark" => ???
				case "isLight" => ???
				case "lighten" => ???
				case "lightness" => ???
				case "red" => Some(Value.Scalar(r))
				case "saturate" => ???
				case "saturation" => ???
			}
			case CurrentColor => None
			case Transparent => ???
		}
		result match {
			case Some(x) => state ~> x
			case None => ???
		}
	}

}

package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression.Expression
import com.mishaLang.ast.Language.Term.MemberAccess
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Boolean, Color, Composite, CurrentColor, Dimensioned, Flag, Callable, NativeFunctionCall, Number, Primitive, Rgba, Scalar, Transparent, Tuple2, Unit}
import com.mishaLang.error.CompilerError
import com.mishaLang.error.ProgramError.NonStringMemberCastFail
import com.mishaLang.interpreter.ops.{ColorOps, ListOps, NumberOps, StringOps}

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
						case composite: Composite => runComposite(composite, string)(newState)
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
			case color: Color => runColor(color, memberName)
			case _: Flag => ???
			case _: NativeFunctionCall => ???
		}


	private def runComposite(composite: Composite, memberName: String)(implicit state: EnvWithValue): Try[EnvWithValue] =
		composite match {
			case Tuple2(first, second) => ???
			case list: Value.List => runList(list, memberName)
			case _: Callable => ???
			case _: Value.Formula => ???
		}


	private def runNumber(number: Value.Number, memberName: String)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val commonResult = memberName match {
			case "isEven" => Some(NumberOps.isEven(number))
			case "isNegative" => Some(NumberOps.isNegative(number))
			case "isOdd" => Some(NumberOps.isOdd(number))
			case "isPositive" => Some(NumberOps.isPositive(number))
			case "isWhole" => Some(NumberOps.isWhole(number))
			case "toPercentage" => Some(NumberOps.toPercentage(number))
			case "toScalar" => Some(NumberOps.toScalar(number))
			case "toString" => StringOps.castToString(number)
			case _ =>
				val newValue = memberName match { // TODO
					case "abs" => number.value.abs
					case "ceil" => number.value.ceil
					case "floor" => number.value.floor
					case "round" => number.value.round
				}
				number match { // Too bad is it impossible to just do number.copy(value = newValue). :/
					case subClass: Dimensioned => Some(subClass.copy(value = newValue))
					case subClass: Scalar => Some(subClass.copy(value = newValue))
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
			case "repeat" => ???
			case "replace" => ???
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
			case rgba: Rgba => memberName match {
				case "adjustHue" => ???
				case "alpha" => Some(ColorOps.alpha(rgba))
				case "blue" => Some(Value.Scalar(rgba.b))
				case "complement" => Some(ColorOps.complement(rgba))
				case "darken" => Some(ColorOps.getDarken(rgba))
				case "desaturate" => ???
				case "green" => Some(Value.Scalar(rgba.g))
				case "hue" => Some(ColorOps.hue(rgba))
				case "inverted" => Some(ColorOps.inverted(rgba))
				case "isDark" => Some(ColorOps.isDark(rgba))
				case "isLight" => Some(ColorOps.isLight(rgba))
				case "lighten" => Some(ColorOps.getLighten(rgba))
				case "lightness" => Some(ColorOps.lightness(rgba))
				case "red" => Some(Value.Scalar(rgba.r))
				case "saturate" => ???
				case "saturation" => Some(ColorOps.saturation(rgba))
			}
			case CurrentColor => None
			case Transparent => ???
		}
		result match {
			case Some(x) => state ~> x
			case None => ???
		}
	}


	private def runList(list: Value.List, memberName: String)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val result = memberName match {
			case "append" => ???
			case "join" => ???
			case "length" => Some(ListOps.length(list))
			case "prepend" => ???
			case _ => None
		}
		result match {
			case Some(x) => state ~> x
			case None => ???
		}
	}

}

package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression.{Addition, Multiplication, NumericOperator, Subtraction}
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value._
import com.mishaLang.error.ProgramError._
import com.mishaLang.interpreter.ops.{ColorOps, ListOps, NumberOps, StringOps}
import com.mishaLang.interpreter.validators.NumberValidator

import scala.util.Try

object NumericOperatorInterpreter {


	def run(operator: NumericOperator, left: Value, right: Value)
				 (implicit state: EnvWithValue): Try[EnvWithValue] = left match {
		case primitive: Primitive => primitive match {
			case number: Value.Number =>
				runOnNumber(operator, number, right)
			case string: Value.String =>
				runOnString(operator, string, right)
			case color: Color =>
				runOnColor(operator, color, right)
			case _: Flag | _: Value.Boolean | _: NativeFunctionCall =>
				state.fail(IllegalNumericOperatorOperand, left, right)
		}
		case composite: Composite => composite match {
			case _: Value.Tuple2 =>
				state.fail(IllegalNumericOperatorOperand, left, right)
			case list: Value.List =>
				runOnList(operator, list, right)
			case formula: Formula => ???
			case _ =>
				state.fail(IllegalNumericOperatorOperand, left, right)
		}
		case Value.Unit =>
			state.fail(IllegalNumericOperatorOperand, left, right)
	}

	private def runOnNumber(operator: NumericOperator, left: Value.Number, right: Value)
												 (implicit state: EnvWithValue): Try[EnvWithValue] =
		right match {
			case primitive: Primitive => primitive match {
				case number: Number =>
					runOnTwoNumbers(operator, left, number)
				case rightString: String =>
					StringOps.castToString(left) match {
						case Some(leftString) =>
							state ~> StringOps.concatenate(leftString, rightString)
						case None =>
							state.fail(IllegalStringConcatenationOperand, left, right)
					}
				case _ =>
					state.fail(IllegalNumericOperatorOperand, left, right)
			}
			case Formula(formula) => ???
			case _ =>
				state.fail(IllegalNumericOperatorOperand, left, right)
		}

	private def runOnTwoNumbers(operator: NumericOperator, left: Value.Number, right: Value.Number)
														 (implicit state: EnvWithValue): Try[EnvWithValue] = {
		(left, right) match {
			case (leftScalar: Value.Scalar, rightScalar: Value.Scalar) =>
				state ~> NumberOps.performNumericOperator(operator, leftScalar, rightScalar)
			case _ => ???
		}
	}

	private def runOnString(operator: NumericOperator, left: Value.String, right: Value)
												 (implicit state: EnvWithValue): Try[EnvWithValue] = operator match {
		case Addition => StringOps.castToString(right) match {
			case Some(string) => state ~> StringOps.concatenate(left, string)
			case None => state.fail(IllegalStringConcatenationOperand, left, right)
		}
		case Multiplication => right match {
			case scalar: Scalar =>
				if (NumberValidator.isInteger(scalar))
					state ~> StringOps.multiply(left, scalar)
				else state.fail(MultiplyingStringByNonInt, left, right)
			case _ => state.fail(MultiplyingStringByNonScalar, left, right)
		}
		case _ => state.fail(IllegalNumericOperatorOperand, left, right)
	}


	private def runOnColor(operator: NumericOperator, left: Color, right: Value)
												(implicit state: EnvWithValue): Try[EnvWithValue] = left match {
		case leftRgba: Rgba => operator match {
			case Addition => right match {
				case rightRgba: Rgba => state ~> ColorOps.addColors(leftRgba, rightRgba)
				case _ => state.fail(IllegalNumericOperatorOperand, left, right)
			}
			case Subtraction => right match {
				case rightRgba: Rgba => state ~> ColorOps.subtractColors(leftRgba, rightRgba)
				case _ => state.fail(IllegalNumericOperatorOperand, left, right)
			}
			case _ => state.fail(IllegalNumericOperatorOperand, left, right)
		}
		case _ => state.fail(IllegalNumericOperatorOperand, left, right)
	}


	private def runOnList(operator: NumericOperator, left: Value.List, right: Value)
											 (implicit state: EnvWithValue): Try[EnvWithValue] = operator match {
		case Addition =>
			right match {
				case list: Value.List => state ~> ListOps.concatenate(left, list)
				case _ => state.fail(IllegalListConcatenationOperand, left, right)
			}
		case Multiplication =>
			right match {
				case Value.Scalar(n) =>
					if (NumberValidator.isInteger(n)) state ~> ListOps.repeat(left, n.toInt)
					else state.fail(NonIntListRepeat, left, right)
				case _ => state.fail(IllegalNumericOperatorOperand, left, right)
			}
		case _ => state.fail(IllegalNumericOperatorOperand, left, right)
	}

}

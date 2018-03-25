package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value.{Boolean, Callable, Color, Composite, Dimensioned, Flag, Formula, NativeFunctionCall, Percentage, Primitive, Rgba, Scalar, String, Tuple2, Value}
import com.mishaLang.ast.Language.{Term, Value}
import com.mishaLang.error.CompilerError
import com.mishaLang.error.ProgramError._
import com.mishaLang.interpreter.ops.{ColorOps, ListOps, NumberOps, StringOps}
import com.mishaLang.interpreter.typing.Subtype
import com.mishaLang.interpreter.validators.NumberValidator

import scala.util.{Failure, Success, Try}

object BinaryOperationInterpreter {

	def run(binaryOperation: BinaryOperation)(implicit state: EnvWithValue): Try[EnvWithValue] = binaryOperation.operator match {
		case _: Assignment => runAssignment(binaryOperation.left, binaryOperation.right)
		case operator: LogicalOperator => runLogicalOperator(operator, binaryOperation.left, binaryOperation.right)
		case _ =>
			val chain = List(binaryOperation.left, binaryOperation.right)

			Interpreter.chainRun[Expression](chain, state, ExpressionInterpreter.run(_)(_)) match {
				case Failure(exception) => Failure(exception)
				case Success(newEnvironment) =>
					val (left :: right :: Nil) = newEnvironment.value
					val newState = EnvironmentWithValue(newEnvironment.environment)

					binaryOperation.operator match {
						case operator: NumericOperator => runNumericOperator(operator, left, right)(newState)
						case operator: Comparison => runComparison(operator, left, right)(newState)
						case _ => state.failFatally(CompilerError("TODO")) // TODO
					}
			}
	}

	private def runNumericOperator(operator: NumericOperator, left: Value, right: Value)
																(implicit state: EnvWithValue): Try[EnvWithValue] = left match {
		case Value.Unit =>
			state.fail(IllegalNumericOperatorOperand, left, right)
		case primitive: Primitive => primitive match {
			case number: Value.Number => number match {
				case Dimensioned(value, unit) =>
					sys.error("todo") // TODO
				case scalar: Scalar =>
					runNumericOperatorOnScalar(operator, scalar, right)
				case Percentage(value) =>
					sys.error("todo") // TODO
			}
			case string: Value.String =>
				runNumericOperatorOnString(operator, string, right)
			case color: Color =>
				runNumericOperatorOnColor(operator, color, right)
			case _: Flag | _: Value.Boolean =>
				state.fail(IllegalNumericOperatorOperand, left, right)
		}
		case composite: Composite => composite match {
			case _: Value.Tuple2 =>
				state.fail(IllegalNumericOperatorOperand, left, right)
			case list: Value.List => operator match {
				case Addition =>
					state ~> ListOps.append(list, right)
				case Multiplication =>
					right match {
						case Value.Scalar(n) =>
							if (NumberValidator.isInteger(n))
								state ~> ListOps.repeat(list, n.toInt)
							else
								state.fail(IllegalNumericOperatorOperand, left, right) // TODO mention explicitly that we'd like an int
						case _ => state.fail(IllegalNumericOperatorOperand, left, right)
					}
				case _ =>
					state.fail(IllegalNumericOperatorOperand, left, right)
			}
			case _ => ??? // TODO
		}
	}

	private def runNumericOperatorOnScalar(operator: NumericOperator, left: Value.Scalar, right: Value)
																				(implicit state: EnvWithValue): Try[EnvWithValue] = right match {
		case primitive: Primitive => primitive match {
			case number: Value.Number => number match {
				case Dimensioned(value, unit) => ???
				case rightScalar: Scalar => state ~> NumberOps.performNumericOperator(operator, left, rightScalar)
				case Percentage(value) => ???
			}
			case Boolean(value) => ???
			case String(value) => ???
			case _: Color => ???
			case _: Flag => ???
			case NativeFunctionCall(function, arguments, returnType) => ???
		}
		case composite: Composite => composite match {
			case Tuple2(first, second) => ???
			case Value.List(values) => ???
			case Formula(formula) => ???
			case _: Callable => ???
		}
		case Value.Unit => ???
	}

	private def runNumericOperatorOnString(operator: NumericOperator, left: Value.String, right: Value)
																				(implicit state: EnvWithValue): Try[EnvWithValue] = operator match {
		case Addition => StringOps.castToString(right) match {
			case Some(string) => state ~> StringOps.concatenate(left, string)
			case None => state.fail(ConcatenatingIllegalOperand, left, right)
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

	private def runNumericOperatorOnColor(operator: NumericOperator, left: Color, right: Value)
																			 (implicit state: EnvWithValue): Try[EnvWithValue] = left match {
		case leftRgba: Rgba => operator match {
			case Addition => right match {
				case percentage: Percentage => state ~> ColorOps.lighten(leftRgba, percentage)
				case rightRgba: Rgba => state ~> ColorOps.addColors(leftRgba, rightRgba)
				case _ => state.fail(IllegalNumericOperatorOperand, left, right)
			}
			case Subtraction => right match {
				case percentage: Percentage => state ~> ColorOps.darken(leftRgba, percentage)
				case rightRgba: Rgba => state ~> ColorOps.subtractColors(leftRgba, rightRgba)
				case _ => state.fail(IllegalNumericOperatorOperand, left, right)
			}
			case _ => state.fail(IllegalNumericOperatorOperand, left, right)
		}
		case _ => state.fail(IllegalNumericOperatorOperand, left, right)
	}

	private def runAssignment(left: Expression, right: Expression)(implicit state: EnvWithValue): Try[EnvWithValue] = left match {
		case Term.Variable(name) => ExpressionInterpreter.run(right) match {
			case Failure(exception) => Failure(exception)
			case Success(newState) =>
				if (newState.environment.isInScope(name)) {
					val value = newState.environment.lookup(name).get

					if (Subtype.isSubtypeOf(newState.value.valueType, value.valueType))
					// TODO add readonly checks
						newState.withUpdatedSymbol(name)(newState.value)
					else newState.fail(IllTypedAssignment)
				}
				else newState.fail(WritingUninitializedVariable, left)
		}
		case _ => state.fail(AssigningToNonVariable, left)
	}

	private def runComparison(operator: Comparison, left: Value, right: Value)
													 (implicit state: EnvWithValue): Try[EnvWithValue] = operator match {
		case IsEqualTo =>
			state ~> Value.Boolean(left == right)
		case In =>
			sys.error("todo") // TODO
		case _ =>
			// Unchecked because it's too stupid to realize that it already can't be IsEqualTo or In
			val operation: (Double, Double) => scala.Boolean = (operator: @unchecked) match {
				case LowerThan => _ < _
				case LowerEquals => _ <= _
				case GreaterThan => _ > _
				case GreaterEquals => _ >= _
			}
			(left, right) match {
				case (leftNumeric: Value.Number, rightNumeric: Value.Number) => (leftNumeric, rightNumeric) match {
					case (Scalar(_), Scalar(_)) |
							 (Percentage(_), Percentage(_)) =>
						state ~> Value.Boolean(operation(leftNumeric.value, rightNumeric.value))
					case (Dimensioned(_, _), Dimensioned(_, _)) => sys.error("todo") // TODO
					case _ => state.fail(ComparingIncompatibleNumerics, left, right)
				}
				case _ => state.fail(ComparingNonNumber, left, right)
			}
	}

	private def runLogicalOperator(operator: LogicalOperator, left: Expression, right: Expression)
																(implicit state: EnvWithValue): Try[EnvWithValue] = {
		ExpressionInterpreter.run(left) match {
			case Failure(exception) => Failure(exception)
			case Success(stateAfterLeft) => ExpressionInterpreter.run(right)(stateAfterLeft) match {
				case Failure(exception) => Failure(exception)
				case Success(stateAfterRight) => (stateAfterLeft.value, stateAfterRight.value) match {
					case (Value.Boolean(leftValue), Value.Boolean(rightValue)) => operator match {
						case LogicalAnd =>
							if (leftValue) stateAfterRight ~> Value.Boolean(rightValue)
							else stateAfterLeft ~> Value.Boolean(leftValue)
						case LogicalOr =>
							if (leftValue) stateAfterLeft ~> Value.Boolean(leftValue)
							else stateAfterRight ~> Value.Boolean(rightValue)
					}
					case (leftNode, rightNode) => stateAfterRight.fail(LogicOnNonBooleans, leftNode, rightNode)
				}
			}
		}
	}

}

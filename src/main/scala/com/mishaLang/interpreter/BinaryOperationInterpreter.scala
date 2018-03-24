package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value.{Boolean, Color, Composite, Dimensioned, Flag, NativeFunctionCall, Percentage, Primitive, Rgba, Scalar, String, Value}
import com.mishaLang.ast.Language.{Term, Value}
import com.mishaLang.error.CompilerError
import com.mishaLang.error.ProgramError._
import com.mishaLang.interpreter.ops.{ColorOps, NumberOps, StringOps}
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
		case Value.Unit => state.fail(IllegalNumericOperatorOperand, left, right)
		case primitive: Primitive => primitive match {
			case number: Value.Number => number match {
				case Dimensioned(value, unit) => sys.error("todo") // TODO
				case scalar: Scalar => runNumericOperatorOnScalar(operator, scalar, right)
				case Percentage(value) => sys.error("todo") // TODO
			}
			case string: Value.String => runNumericOperatorOnString(operator, string, right)
			case color: Color => runNumericOperatorOnColor(operator, color, right)
			case _: Flag | _: Value.Boolean => state.fail(IllegalNumericOperatorOperand, left, right)
		}
		case composite: Composite => composite match {
			case _: Value.Tuple2 => state.fail(IllegalNumericOperatorOperand, left, right)
			case Value.List(values) => operator match {
				case Addition => sys.error("todo") // TODO New element of a correct type or a list
				case Multiplication => sys.error("todo") // TODO Non-negative integer scalar
				case _ => state.fail(IllegalNumericOperatorOperand, left, right)
			}
			case _ => ??? // TODO
		}
	}

	private def runNumericOperatorOnScalar(operator: NumericOperator, left: Value.Scalar, right: Value)
																				(implicit state: EnvWithValue): Try[EnvWithValue] = right match {
		case Value.Unit => ???
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
		case _: Composite => ???
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

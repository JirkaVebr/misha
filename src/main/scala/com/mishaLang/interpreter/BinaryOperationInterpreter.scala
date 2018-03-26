package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value.{Dimensioned, Percentage, Scalar, Value}
import com.mishaLang.ast.Language.{Term, Value}
import com.mishaLang.error.CompilerError
import com.mishaLang.error.ProgramError._
import com.mishaLang.interpreter.typing.{Subtype, Typing}

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
						case operator: NumericOperator =>
							NumericOperatorInterpreter.run(operator, left, right)(newState)
						case operator: Comparison => runComparison(operator, left, right)(newState)
						case _ => state.failFatally(CompilerError("TODO")) // TODO
					}
			}
	}


	private def runAssignment(left: Expression, right: Expression)(implicit state: EnvWithValue): Try[EnvWithValue] = left match {
		case Term.Variable(name) => ExpressionInterpreter.run(right) match {
			case Failure(exception) => Failure(exception)
			case Success(newState) =>
				if (newState.environment.isInScope(name)) {
					val value = newState.environment.lookup(name).get

					if (Typing.canBeAssignedTo(newState.value, value.valueType))
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

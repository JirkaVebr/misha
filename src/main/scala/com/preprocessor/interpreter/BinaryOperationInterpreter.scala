package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Value.{Color, Composite, Flag, Number, Primitive, Rgba, String, Transparent, Tuple2Value, Unit}
import com.preprocessor.ast.Ast.{Term, Value}
import com.preprocessor.ast.UnitOfMeasure.{GenericUnit, Percentage, Scalar}
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.CompilerError
import com.preprocessor.error.ProgramError._

import scala.util.{Failure, Success, Try}

object BinaryOperationInterpreter {

	def run(binaryOperation: BinaryOperation)(implicit state: EvalState): Try[EvalState] = binaryOperation.operator match {
		case _: Assignment => runAssignment(binaryOperation.left, binaryOperation.right)
		case operator: LogicalOperator => runLogicalOperator(operator, binaryOperation.left, binaryOperation.right)
		case _ =>
			val chain = List(binaryOperation.left, binaryOperation.right)

			Interpreter.chainRun[Expression](chain, state, ExpressionInterpreter.run(_)(_)) match {
				case Failure(exception) => Failure(exception)
				case Success((left :: right :: Nil, newState)) => binaryOperation.operator match {
					case operator: NumericOperator => runNumericOperator(operator, left, right)(newState)
					case operator: Comparison => runComparison(operator, left, right)(newState)
					case _ => state.failFatally(CompilerError()) // TODO
				}
				case _ => state.failFatally(CompilerError()) // TODO
			}

		/*
			StringOperations.castToString(right) match {
				case Some(string) => newState ~> string
				case None => newState.fail(ProgramError.ConcatenatingIllegalOperand)
			}
		 */
	}

	private def runNumericOperator(operator: NumericOperator, left: ValueRecord, right: ValueRecord)
																(implicit state: EvalState): Try[EvalState] = left.value match {
		case Unit => state.fail(IllegalNumericOperatorOperand, left.value, right.value)
		case primitive: Primitive => primitive match {
			case Number(value, unit) => sys.error("todo") // TODO
			case String(value) => sys.error("todo") // TODO Plus string or multiply by non-negative integer scalar
			case color: Color => color match {
				case rgba: Rgba => operator match {
					case Addition => sys.error("todo") // TODO rgba or percentage
					case Subtraction => sys.error("todo")
					case _ => state.fail(IllegalNumericOperatorOperand, left.value, right.value)
				}
				case _ => state.fail(IllegalNumericOperatorOperand, left.value, right.value)
			}
			case _: Flag | _: Value.Boolean => state.fail(IllegalNumericOperatorOperand, left.value, right.value)
		}
		case composite: Composite => composite match {
			case _: Tuple2Value => state.fail(IllegalNumericOperatorOperand, left.value, right.value)
			case Value.List(values) => operator match {
				case Addition => sys.error("todo") // TODO New element of a correct type or a list
				case Multiplication => sys.error("todo") // TODO Non-negative integer scalar
				case _ => state.fail(IllegalNumericOperatorOperand, left.value, right.value)
			}
		}
	}

	private def runAssignment(left: Expression, right: Expression)(implicit state: EvalState): Try[EvalState] = left match {
		case Term.Variable(name) => ExpressionInterpreter.run(right) match {
			case Failure(exception) => Failure(exception)
			case Success(newState) =>
				if (newState.environment.isInScope(name)) {
					val valueRecord = newState.environment.lookup(name).get

					if (newState.valueRecord.recordType isSubtypeOf valueRecord.recordType)
					// TODO add readonly checks
						newState.withUpdatedSymbol(name)(valueRecord ~> newState.valueRecord.value)
					else newState.fail(IllTypedAssignment)
				}
				else newState.fail(WritingUninitializedVariable, left)
		}
		case _ => state.fail(AssigningToNonVariable, left)
	}

	private def runComparison(operator: Comparison, left: ValueRecord, right: ValueRecord)
													 (implicit state: EvalState): Try[EvalState] = operator match {
		case IsEqualTo =>
			state evaluatedTo Value.Boolean(left.value == right.value)
		case In =>
			sys.error("todo") // TODO
		case _ =>
			// Unchecked because it's too stupid to realize that it already can't be IsEqualTo or In
			val operation: (Double, Double) => Boolean = (operator: @unchecked) match {
				case LowerThan => _ < _
				case LowerEquals => _ <= _
				case GreaterThan => _ > _
				case GreaterEquals => _ >= _
			}
			(left.value, right.value) match {
				case (Value.Number(valueLeft, unitLeft), Value.Number(valueRight, unitRight)) =>
					(unitLeft, unitRight) match {
						case (Percentage, Percentage) | (Scalar, Scalar) =>
							state evaluatedTo Value.Boolean(operation(valueLeft, valueRight))
						case (GenericUnit(_), GenericUnit(_)) => sys.error("todo") // TODO
						case _ => state.fail(ComparingIncompatibleNumerics, left.value, right.value)
					}
				case _ => state.fail(ComparingNonNumber, left.value, right.value)
			}
	}

	private def runLogicalOperator(operator: LogicalOperator, left: Expression, right: Expression)
																(implicit state: EvalState): Try[EvalState] = {
		ExpressionInterpreter.run(left) match {
			case Failure(exception) => Failure(exception)
			case Success(stateAfterLeft) => ExpressionInterpreter.run(right)(stateAfterLeft) match {
				case Failure(exception) => Failure(exception)
				case Success(stateAfterRight) => (stateAfterLeft.valueRecord.value, stateAfterRight.valueRecord.value) match {
					case (Value.Boolean(leftValue), Value.Boolean(rightValue)) => operator match {
						case LogicalAnd =>
							if (leftValue) stateAfterRight evaluatedTo Value.Boolean(rightValue)
							else stateAfterLeft evaluatedTo Value.Boolean(leftValue)
						case LogicalOr =>
							if (leftValue) stateAfterLeft evaluatedTo Value.Boolean(leftValue)
							else stateAfterRight evaluatedTo Value.Boolean(rightValue)
					}
					case (leftNode, rightNode) => stateAfterRight.fail(LogicOnNonBooleans, leftNode, rightNode)
				}
			}
		}
	}

}

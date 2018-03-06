package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression._
import com.preprocessor.ast.Language.Term.Term
import com.preprocessor.ast.Language.Value.{Dimensioned, Percentage, Scalar}
import com.preprocessor.ast.Language.{Type, Value}
import com.preprocessor.error.ProgramError
import com.preprocessor.error.ProgramError.{IllTypedConditionBranches, NonBooleanCondition}
import com.preprocessor.interpreter.typing.Subtype

import scala.util.{Failure, Success, Try}

object ExpressionInterpreter {

	def run(expression: Expression)(implicit state: EvalState): Try[EvalState] = expression match {
		case binaryOperation: BinaryOperation => BinaryOperationInterpreter.run(binaryOperation)
		case unaryOperation: UnaryOperation => runUnaryOperation(unaryOperation)
		case conditional: Conditional => runConditional(conditional)
		case StringInterpolation(components) => sys.error("todo") // TODO
		case block: Block => runBlock(block)
		case term: Term => TermInterpreter.run(term)
	}

	private def runUnaryOperation(unaryOperation: UnaryOperation)(implicit state: EvalState): Try[EvalState] = {
		val operator = unaryOperation.operator
		val value = run(unaryOperation.value)

		value match {
			case Failure(_) => value
			case Success(newState) => operator match {
				case LogicalNegation => newState.valueRecord.value match {
					case Value.Boolean(bool) => newState evaluatedTo Value.Boolean(!bool)
					case _ => newState.fail(ProgramError.NegatingNonBoolean)
				}
				case ArithmeticNegation => newState.valueRecord.value match {
					case number: Value.Number =>  newState evaluatedTo (number match {
						case Dimensioned(magnitude, unit) => Dimensioned(-magnitude, unit)
						case Scalar(magnitude) => Scalar(-magnitude)
						case Percentage(magnitude) => Percentage(-magnitude)
					})
					case _ => newState.fail(ProgramError.NegatingNonBoolean)
				}
			}
		}
	}

	private def runConditional(conditional: Conditional)(implicit state: EvalState): Try[EvalState] = {
		run(conditional.condition) match {
			case Failure(error) => Failure(error)
			case Success(stateAfterCondition) => stateAfterCondition.valueRecord.value match {
				case Value.Boolean(conditionValue) =>
					run(conditional.consequent)(stateAfterCondition) match {
						case Failure(error) => Failure(error)
						case Success(stateAfterConsequent) => conditional.alternative match {
							case Some(alternative) =>
								run(alternative)(stateAfterCondition) match {
									case Failure(error) => Failure(error)
									case Success(stateAfterAlternative) =>
										if (stateAfterConsequent.valueRecord.recordType isEquivalentTo
											stateAfterAlternative.valueRecord.recordType) {
											val superType =
												Subtype.getLowestCommonSupertype(
													stateAfterConsequent.valueRecord.recordType, stateAfterAlternative.valueRecord.recordType
												)
											if (conditionValue) stateAfterConsequent ~> superType
											else stateAfterAlternative ~> superType
										} else
											stateAfterConsequent.fail(IllTypedConditionBranches)
								}
							case None =>
								(if (conditionValue) stateAfterConsequent else stateAfterCondition) ~> Type.Unit
						}
					}
				case _ => stateAfterCondition.fail(NonBooleanCondition)
			}
		}
	}

	private def runBlock(block: Block)(implicit state: EvalState): Try[EvalState] = {
		val newScope = state.environment.pushSubScope()
		StatementInterpreter.run(block.content)(EvalState(newScope)) match {
			case fail: Failure[EvalState] => fail
			case Success(result) =>
				Success(EvalState(result.environment.popSubScope().get, result.valueRecord))
		}
	}
}

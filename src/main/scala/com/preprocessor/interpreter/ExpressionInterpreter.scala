package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Term.Term
import com.preprocessor.ast.Ast.{Type, Value}
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
		case term: Term => TermInterpreter.run(term)
	}

	private def runUnaryOperation(unaryOperation: UnaryOperation)(implicit state: EvalState): Try[EvalState] = {
		val operator = unaryOperation.operator
		val value = run(unaryOperation.value)

		value match {
			case Failure(_) => value
			case Success(newState) => operator match {
				case LogicalNegation => newState.value match {
					case Value.Boolean(bool) => newState ~> Value.Boolean(!bool)
					case _ => newState.fail(ProgramError.NegatingNonBoolean)
				}
				case ArithmeticNegation => newState.value match {
					case Value.Number(magnitude, unit) => newState ~> Value.Number(-magnitude, unit)
					case _ => newState.fail(ProgramError.NegatingNonBoolean)
				}
			}
		}
	}

	private def runConditional(conditional: Conditional)(implicit state: EvalState): Try[EvalState] = {
		run(conditional.condition) match {
			case Failure(error) => Failure(error)
			case Success(stateAfterCondition) => stateAfterCondition.value match {
				case Value.Boolean(conditionValue) =>
					run(conditional.consequent)(stateAfterCondition) match {
						case Failure(error) => Failure(error)
						case Success(stateAfterConsequent) => conditional.alternative match {
							case Some(alternative) =>
								run(alternative)(stateAfterCondition) match {
									case Failure(error) => Failure(error)
									case Success(stateAfterAlternative) =>
										if (stateAfterConsequent.nodeType isEquivalentTo stateAfterAlternative.nodeType) {
											val superType =
												Subtype.getLowestCommonSupertype(stateAfterConsequent.nodeType, stateAfterAlternative.nodeType)
											if (conditionValue) stateAfterConsequent withNewType superType
											else stateAfterAlternative withNewType superType
										} else
											stateAfterConsequent.fail(IllTypedConditionBranches)
								}
							case None =>
								(if (conditionValue) stateAfterConsequent else stateAfterCondition) withNewType Type.Unit
						}
					}
				case _ => stateAfterCondition.fail(NonBooleanCondition)
			}
		}

	}
}

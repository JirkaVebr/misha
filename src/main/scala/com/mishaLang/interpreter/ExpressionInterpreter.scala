package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Term.Term
import com.mishaLang.ast.Language.Value
import com.mishaLang.error.ProgramError
import com.mishaLang.error.ProgramError.{NonBooleanCondition, StackOverflow}

import scala.util.{Failure, Success, Try}

object ExpressionInterpreter {

	def run(expression: Expression)(implicit state: EnvWithValue): Try[EnvWithValue] = expression match {
		case binaryOperation: BinaryOperation => BinaryOperationInterpreter.run(binaryOperation)
		case unaryOperation: UnaryOperation => runUnaryOperation(unaryOperation)
		case conditional: Conditional => runConditional(conditional)
		case block: Block => runBlock(block)
		case term: Term => TermInterpreter.run(term)
	}


	private def runUnaryOperation(unaryOperation: UnaryOperation)(implicit state: EnvWithValue): Try[EnvWithValue] = {
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
					case number: Value.Number => newState ~> number.copy(value = -number.value)
					// TODO do the same for formulas
					case _ => newState.fail(ProgramError.NegatingNonBoolean)
				}
			}
		}
	}


	private def runConditional(conditional: Conditional)(implicit state: EnvWithValue): Try[EnvWithValue] =
		run(conditional.condition) match {
			case Failure(error) => Failure(error)
			case Success(stateAfterCondition) => stateAfterCondition.value match {
				case Value.Boolean(conditionValue) =>
					if (conditionValue)
						run(conditional.consequent)(stateAfterCondition)
					else
						conditional.alternative match {
							case Some(alternative) => run(alternative)(stateAfterCondition)
							case None => stateAfterCondition ~> Value.Unit
						}
				case _ => stateAfterCondition.fail(NonBooleanCondition)
			}
		}


	def runBlock(block: Block)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		state.environment.pushSubScope() match {
			case Some(newEnvironment) =>
				StatementInterpreter.run(block.content)(EnvironmentWithValue(newEnvironment)) match {
					case fail: Failure[EnvWithValue] => fail
					case Success(result) =>
						Success(EnvironmentWithValue(result.environment.popSubScope().get, result.value))
				}
			case None =>
				state.fail(StackOverflow)
		}
	}
}
